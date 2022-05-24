module type CONFIG = sig
  type persistent_state
  type session_state

  val base_name : string
  val default_colors : Tezt.Log.Color.t array
end

module Level = struct
  type default =
    [ `Debug
    | `Info
    | `Notice
    ]

  type t =
    [ default
    | `Warning
    | `Error
    | `Fatal
    ]

  let to_string = function
    | `Debug -> "debug"
    | `Info -> "info"
    | `Notice -> "notice"
    | `Warning -> "warning"
    | `Error -> "error"
    | `Fatal -> "fatal"
  ;;
end

module Make (C : CONFIG) () = struct
  module Session = struct
    type status =
      { process : Tezt.Process.t
      ; session_state : C.session_state
      ; mutable event_loop : unit Lwt.t option
      }
  end

  module Event = struct
    exception
      Terminated_before of
        { node : string
        ; event : string
        ; where : string option
        }

    type handler =
      | Handler :
          { filter : Tezt.JSON.t -> 'a option
          ; resolver : 'a option Lwt.u
          }
          -> handler

    type t =
      { name : string
      ; value : Tezt.JSON.t
      }

    let () =
      Printexc.register_printer (function
          | Terminated_before { node; event; where = None } ->
            Some
              (Format.sprintf
                 "%s terminated before event occurred: %s"
                 node
                 event)
          | Terminated_before { node; event; where = Some where } ->
            Some
              (Format.sprintf
                 "%s terminated before event occurred: %s where %s"
                 node
                 event
                 where)
          | _ -> None)
    ;;
  end

  type status =
    | Not_running
    | Running of Session.status

  type t =
    { name : string
    ; color : Tezt.Log.Color.t
    ; path : string
    ; persistent_state : C.persistent_state
    ; event_pipe : string
    ; mutable status : status
    ; mutable stdout_handlers : (string -> unit) list
    ; mutable persistent_event_handlers : (Event.t -> unit) list
    ; mutable event_handlers : Event.handler list Tezt.Base.String_map.t
    }

  let next_name_ref = ref 1
  let next_color_ref = ref 0

  let () =
    Tezt.Test.declare_reset_function (fun () ->
        next_name_ref := 1;
        next_color_ref := 0)
  ;;

  let fresh_name () =
    let i = !next_name_ref in
    let () = incr next_name_ref in
    Format.sprintf "%s%d" C.base_name i
  ;;

  let next_color () =
    let c = !next_color_ref in
    let l = Array.length C.default_colors in
    let i = c mod l in
    let () = incr next_color_ref in
    C.default_colors.(i)
  ;;

  let terminate ?(kill = false) node =
    match node.status with
    | Not_running -> Lwt.return_unit
    | Running Session.{ event_loop = None; _ } ->
      invalid_arg "The node was not started properly"
    | Running Session.{ process; event_loop = Some event_loop; _ } ->
      let () = if kill then Tezt.Process.kill process in
      event_loop
  ;;

  let get_event_from_full_event json =
    let event = Tezt.JSON.(json |-> "fd-sink-item.v0" |-> "event") in
    match Tezt.JSON.as_object_opt event with
    | None | Some ([] | _ :: _ :: _) -> None
    | Some [ (name, value) ] -> Some Event.{ name; value }
  ;;

  let handle_raw_event node line =
    let json = Tezt.JSON.parse ~origin:("event from " ^ node.name) line in
    match get_event_from_full_event json with
    | None -> ()
    | Some raw_event ->
      let name = raw_event.Event.name in
      List.iter
        (fun handler -> handler raw_event)
        node.persistent_event_handlers;
      (match Tezt.Base.String_map.find_opt name node.event_handlers with
      | None -> ()
      | Some events ->
        let rec loop acc = function
          | [] ->
            node.event_handlers
              <- Tezt.Base.String_map.add
                   name
                   (List.rev acc)
                   node.event_handlers
          | (Event.Handler { filter; resolver } as head) :: tail ->
            let acc =
              match filter json with
              | None -> head :: acc
              | Some value ->
                Lwt.wakeup_later resolver (Some value);
                acc
              | exception exn ->
                Tezt.Test.fail
                  "uncaught exception in filter for event %s of daemon %s: %s"
                  name
                  node.name
                  (Printexc.to_string exn)
            in
            loop acc tail
        in
        loop [] events)
  ;;

  let create ~path ?runner ?name ?color ?event_pipe persistent_state =
    let name = (Option.fold ~some:Fun.const ~none:fresh_name name) () in
    let color = (Option.fold ~some:Fun.const ~none:next_color color) () in
    let event_pipe =
      match event_pipe with
      | None -> Tezt.Temp.file ?runner (name ^ "-event-pipe")
      | Some file -> file
    in
    { name
    ; color
    ; path
    ; persistent_state
    ; status = Not_running
    ; event_pipe
    ; stdout_handlers = []
    ; persistent_event_handlers = []
    ; event_handlers = Tezt.Base.String_map.empty
    }
  ;;

  let run
      ?runner
      ?(on_terminate = fun _ -> Lwt.return_unit)
      ?(event_level = `Info)
      ?(event_sections_levels = [])
      node
      session_state
      arguments
    =
    let () =
      match node.status with
      | Running _ -> Tezt.Test.fail "%s is already runnig" node.name
      | Not_running -> ()
    in
    let () =
      if Tezt.Runner.Sys.file_exists ?runner node.event_pipe
      then Tezt.Runner.Sys.remove ?runner node.event_pipe
    in
    let () = Tezt.Runner.Sys.mkfifo ?runner ~perms:0o640 node.event_pipe in
    let event_process =
      match runner with
      | None -> None
      | Some runner ->
        let cmd = "cat" in
        let arguments = [ node.event_pipe ] in
        let name = Filename.basename node.event_pipe in
        let process =
          Tezt.Process.spawn ~name ~runner ~log_output:false cmd arguments
        in
        Some process
    in
    let open Lwt.Syntax in
    let* event_input =
      match event_process with
      | None -> Lwt_io.(open_file ~mode:input) node.event_pipe
      | Some process -> Lwt.return @@ Tezt.Process.stdout process
    in
    let env =
      let args =
        List.fold_right
          (fun (prefix, level) args ->
            Format.sprintf "section-prefix=%s:%s" prefix (Level.to_string level)
            :: args)
          (("", (event_level :> Level.t)) :: event_sections_levels)
          []
      in
      let args_str = "?" ^ String.concat "&" (List.rev args) in
      Tezt.Base.String_map.singleton
        "TEZOS_EVENTS_CONFIG"
        ("file-descriptor-path://" ^ node.event_pipe ^ args_str)
    in
    let process =
      Tezt.Process.spawn
        ?runner
        ~name:node.name
        ~color:node.color
        ~env
        node.path
        arguments
    in
    let running_status =
      Session.{ process; session_state; event_loop = None }
    in
    let () = node.status <- Running running_status in
    let event_loop =
      let rec event_loop () =
        let* line = Lwt_io.read_line_opt event_input in
        match line with
        | Some line ->
          handle_raw_event node line;
          event_loop ()
        | None ->
          (match node.status with
          | Not_running ->
            (match event_process with
            | None -> Lwt_io.close event_input
            | Some process -> Lwt.return (Tezt.Process.kill process))
          | Running _ ->
            let* () = Lwt_unix.sleep 0.01 in
            event_loop ())
      in
      let rec stdout_loop () =
        let* stdout_line = Lwt_io.read_line_opt (Tezt.Process.stdout process) in
        match stdout_line with
        | Some line ->
          List.iter (fun handler -> handler line) node.stdout_handlers;
          stdout_loop ()
        | None ->
          (match node.status with
          | Not_running -> Lwt.return_unit
          | Running _ ->
            let* () = Lwt_unix.sleep 0.01 in
            stdout_loop ())
      in
      let ( and*! ) = Tezt.Base.lwt_both_fail_early in
      let* () = event_loop ()
      and*! () = stdout_loop ()
      and*! () =
        let* process_status = Tezt.Process.wait process in
        node.status <- Not_running;
        let pending = node.event_handlers in
        node.event_handlers <- Tezt.Base.String_map.empty;
        Tezt.Base.String_map.iter
          (fun _ ->
            List.iter (fun (Event.Handler { resolver; _ }) ->
                Lwt.wakeup_later resolver None))
          pending;
        on_terminate process_status
      in
      Lwt.return_unit
    in
    let () = running_status.event_loop <- Some event_loop in
    let () = Tezt.Background.register event_loop in
    Lwt.return_unit
  ;;

  let wait_for_full ?where node name filter =
    let open Lwt.Syntax in
    let promise, resolver = Lwt.task () in
    let current_events =
      Tezt.Base.String_map.find_opt name node.event_handlers
      |> Option.value ~default:[]
    in
    let new_map =
      Tezt.Base.String_map.add
        name
        (Event.Handler { filter; resolver } :: current_events)
        node.event_handlers
    in
    let () = node.event_handlers <- new_map in
    let* result = promise in
    match result with
    | None ->
      raise (Event.Terminated_before { node = node.name; event = name; where })
    | Some x -> Lwt.return x
  ;;

  let event_from_filter filter json =
    let raw_event = get_event_from_full_event json in
    Option.bind raw_event (fun { value; _ } -> filter value)
  ;;

  let wait_for ?where node name filter =
    wait_for_full ?where node name (event_from_filter filter)
  ;;

  let on_event node handler =
    let new_handlers = handler :: node.persistent_event_handlers in
    node.persistent_event_handlers <- new_handlers
  ;;

  let on_stdout node handler =
    let new_handlers = handler :: node.stdout_handlers in
    node.stdout_handlers <- new_handlers
  ;;

  let log_events node =
    let handler event =
      let name = event.Event.name
      and value = event.Event.value in
      Tezt.Log.info "Received event: %s -> %s" name (Tezt.JSON.encode value)
    in
    on_event node handler
  ;;
end

module Tezos = struct
  type history_mode =
    | Archive
    | Full of int option
    | Rolling of int option

  type media_type =
    | Json
    | Binary
    | Any

  let string_of_media_type = function
    | Any -> "any"
    | Binary -> "binary"
    | Json -> "json"
  ;;

  type argument =
    | Network of string
    | History_mode of history_mode
    | Expected_pow of int
    | Singleprocess
    | Bootstrap_threshold of int
    | Synchronisation_threshold of int
    | Connections of int
    | Private_mode
    | Peer of string
    | No_bootstrap_peers
    | Disable_operations_precheck
    | Media_type of media_type
    | Metadata_size_limit of int option
    | Metrics_addr of string

  let make_argument = function
    | Network x -> [ "--network"; x ]
    | History_mode Archive -> [ "--history-mode"; "archive" ]
    | History_mode (Full None) -> [ "--history-mode"; "full" ]
    | History_mode (Full (Some i)) ->
      [ "--history-mode"; "full:" ^ string_of_int i ]
    | History_mode (Rolling None) -> [ "--history-mode"; "rolling" ]
    | History_mode (Rolling (Some i)) ->
      [ "--history-mode"; "rolling:" ^ string_of_int i ]
    | Expected_pow x -> [ "--expected-pow"; string_of_int x ]
    | Singleprocess -> [ "--singleprocess" ]
    | Bootstrap_threshold x -> [ "--bootstrap-threshold"; string_of_int x ]
    | Synchronisation_threshold x ->
      [ "--synchronisation-threshold"; string_of_int x ]
    | Connections x -> [ "--connections"; string_of_int x ]
    | Private_mode -> [ "--private-mode" ]
    | Peer x -> [ "--peer"; x ]
    | No_bootstrap_peers -> [ "--no-bootstrap-peers" ]
    | Disable_operations_precheck -> [ "--disable-mempool-precheck" ]
    | Media_type media_type ->
      [ "--media-type"; string_of_media_type media_type ]
    | Metadata_size_limit None -> [ "--metadata-size-limit"; "unlimited" ]
    | Metadata_size_limit (Some i) ->
      [ "--metadata-size-limit"; string_of_int i ]
    | Metrics_addr metrics_addr -> [ "--metrics-addr"; metrics_addr ]
  ;;

  let make_arguments arguments = List.flatten (List.map make_argument arguments)

  module Conf = struct
    type persistent_state =
      { data_dir : string
      ; mutable net_port : int
      ; advertised_net_port : int option
      ; rpc_host : string
      ; rpc_port : int
      ; default_expected_pow : int
      ; mutable arguments : argument list
      ; mutable pending_ready : unit option Lwt.u list
      ; mutable pending_level : (int * int option Lwt.u) list
      ; mutable pending_identity : string option Lwt.u list
      ; runner : Tezt.Runner.t option
      }

    type session_state =
      { mutable ready : bool
      ; mutable level : int option
      ; mutable identity : string option
      }

    let base_name = "node"

    let default_colors =
      Tezt.Log.Color.[| FG.cyan; FG.magenta; FG.yellow; FG.green |]
    ;;
  end

  module N = Make (Conf) ()

  type t = N.t
  type event = N.Event.t

  let wait_for_full = N.wait_for_full
  let wait_for = N.wait_for
  let on_event = N.on_event
  let on_stdout = N.on_stdout
  let log_events = N.log_events
  let terminate = N.terminate
  let name node = node.N.name

  let check_error ?exit_code ?msg node =
    match node.N.status with
    | N.Not_running ->
      Tezt.Test.fail "node %s is not running, it has no stderr" (name node)
    | N.Running { process; _ } ->
      Tezt.Process.check_error ?exit_code ?msg process
  ;;

  let wait node =
    match node.N.status with
    | N.Not_running ->
      Tezt.Test.fail
        "node %s is not running, cannot wait for it to terminate"
        (name node)
    | N.Running { process; _ } -> Tezt.Process.wait process
  ;;

  let net_port node = node.N.persistent_state.net_port
  let advertised_net_port node = node.N.persistent_state.advertised_net_port
  let rpc_host node = node.N.persistent_state.rpc_host
  let rpc_port node = node.N.persistent_state.rpc_port
  let data_dir node = node.N.persistent_state.data_dir
  let runner node = node.N.persistent_state.runner

  let spawn_command node =
    Tezt.Process.spawn
      ?runner:node.N.persistent_state.runner
      ~name:node.N.name
      ~color:node.N.color
      node.N.path
  ;;

  let spawn_identity_generate ?expected_pow node =
    spawn_command
      node
      [ "identity"
      ; "generate"
      ; "--data-dir"
      ; node.persistent_state.data_dir
      ; string_of_int
          (Option.value
             expected_pow
             ~default:node.N.persistent_state.default_expected_pow)
      ]
  ;;

  let identity_generate ?expected_pow node =
    spawn_identity_generate ?expected_pow node |> Tezt.Process.check
  ;;

  let spawn_config_init node arguments =
    let arguments = node.N.persistent_state.arguments @ arguments in
    node.N.persistent_state.arguments <- [];
    let arguments =
      if List.exists
           (function
             | Network _ -> true
             | _ -> false)
           arguments
      then arguments
      else Network "sandbox" :: arguments
    in
    spawn_command
      node
      ("config"
      :: "init"
      :: "--data-dir"
      :: node.N.persistent_state.data_dir
      :: make_arguments arguments)
  ;;

  let config_init node arguments =
    spawn_config_init node arguments |> Tezt.Process.check
  ;;

  let trigger_ready node value =
    let pending = node.N.persistent_state.pending_ready in
    node.N.persistent_state.pending_ready <- [];
    List.iter (fun pending -> Lwt.wakeup_later pending value) pending
  ;;

  let set_ready node =
    (match node.N.status with
    | N.Not_running -> ()
    | N.Running status -> status.session_state.ready <- true);
    trigger_ready node (Some ())
  ;;

  let update_level node current_level =
    (match node.N.status with
    | N.Not_running -> ()
    | N.Running status ->
      (match status.session_state.level with
      | None -> status.session_state.level <- Some current_level
      | Some old_level ->
        status.session_state.level <- Some (max old_level current_level)));
    let pending = node.persistent_state.pending_level in
    node.persistent_state.pending_level <- [];
    List.iter
      (fun ((level, resolver) as pending) ->
        if current_level >= level
        then Lwt.wakeup_later resolver (Some current_level)
        else
          node.persistent_state.pending_level
            <- pending :: node.persistent_state.pending_level)
      pending
  ;;

  let update_identity node identity =
    match node.N.status with
    | Not_running -> ()
    | Running status ->
      (match status.session_state.identity with
      | None -> status.session_state.identity <- Some identity
      | Some identity' ->
        if identity' <> identity then Tezt.Test.fail "node identity changed");
      let pending = node.persistent_state.pending_identity in
      node.persistent_state.pending_identity <- [];
      List.iter
        (fun resolver -> Lwt.wakeup_later resolver (Some identity))
        pending
  ;;

  let handle_event node N.Event.{ name; value } =
    match name with
    | "node_is_ready.v0" -> set_ready node
    | "node_chain_validator.v0" ->
      (match Tezt.JSON.as_list_opt value with
      | Some [ _timestamp; details ] ->
        (match
           Tezt.JSON.(
             details |-> "event" |-> "processed_block" |-> "level" |> as_int_opt)
         with
        | None -> ()
        | Some level -> update_level node level)
      | _ -> ())
    | "read_identity.v0" -> update_identity node (Tezt.JSON.as_string value)
    | _ -> ()
  ;;

  let check_event ?where node name promise =
    let open Lwt.Syntax in
    let* result = promise in
    match result with
    | None ->
      raise
        (N.Event.Terminated_before { node = node.N.name; event = name; where })
    | Some x -> Lwt.return x
  ;;

  let wait_for_ready node =
    match node.N.status with
    | Running { session_state = { ready = true; _ }; _ } -> Lwt.return_unit
    | Not_running | Running { session_state = { ready = false; _ }; _ } ->
      let promise, resolver = Lwt.task () in
      node.persistent_state.pending_ready
        <- resolver :: node.persistent_state.pending_ready;
      check_event node "node_is_ready.v0" promise
  ;;

  let wait_for_level node level =
    match node.N.status with
    | Running { session_state = { level = Some current_level; _ }; _ }
      when current_level >= level -> Lwt.return current_level
    | Not_running | Running _ ->
      let promise, resolver = Lwt.task () in
      node.persistent_state.pending_level
        <- (level, resolver) :: node.persistent_state.pending_level;
      check_event
        node
        "node_chain_validator.v0"
        ~where:("level >= " ^ string_of_int level)
        promise
  ;;

  let get_level node =
    match node.N.status with
    | Running { session_state = { level = Some level; _ }; _ } -> level
    | Not_running | Running _ -> 0
  ;;

  let wait_for_identity node =
    match node.N.status with
    | Running { session_state = { identity = Some identity; _ }; _ } ->
      Lwt.return identity
    | Not_running | Running _ ->
      let promise, resolver = Lwt.task () in
      node.persistent_state.pending_identity
        <- resolver :: node.persistent_state.pending_identity;
      check_event node "read_identity.v0" promise
  ;;

  let wait_for_request ~request node =
    let event_name =
      match request with
      | `Flush | `Inject -> "request_completed_notice.v0"
      | `Notify | `Arrived -> "request_completed_debug.v0"
    in
    let request_str =
      match request with
      | `Flush -> "flush"
      | `Inject -> "inject"
      | `Notify -> "notify"
      | `Arrived -> "arrived"
    in
    let filter json =
      match Tezt.JSON.(json |-> "view" |-> "request" |> as_string_opt) with
      | Some s when String.equal s request_str -> Some ()
      | Some _ | None -> None
    in
    N.wait_for node event_name filter
  ;;

  let create
      ?runner
      ?(path = Constants.tezos_node)
      ?name
      ?color
      ?data_dir
      ?event_pipe
      ?net_port
      ?advertised_net_port
      ?(rpc_host = "localhost")
      ?rpc_port
      arguments
    =
    let name =
      match name with
      | None -> N.fresh_name ()
      | Some name -> name
    in
    let data_dir =
      match data_dir with
      | None -> Tezt.Temp.dir ?runner name
      | Some dir -> dir
    in
    let net_port =
      match net_port with
      | None -> Port.fresh ()
      | Some port -> port
    in
    let rpc_port =
      match rpc_port with
      | None -> Port.fresh ()
      | Some port -> port
    in
    let arguments =
      if List.exists
           (function
             | Expected_pow _ -> true
             | _ -> false)
           arguments
      then arguments
      else Expected_pow 0 :: arguments
    in
    let default_expected_pow =
      Tezt.Base.list_find_map
        (function
          | Expected_pow x -> Some x
          | _ -> None)
        arguments
      |> Option.value ~default:0
    in
    let node =
      N.create
        ?runner
        ~path
        ~name
        ?color
        ?event_pipe
        { data_dir
        ; net_port
        ; advertised_net_port
        ; rpc_host
        ; rpc_port
        ; arguments
        ; default_expected_pow
        ; runner
        ; pending_ready = []
        ; pending_level = []
        ; pending_identity = []
        }
    in
    N.on_event node (handle_event node);
    node
  ;;

  let add_argument node argument =
    let new_arguments = argument :: node.N.persistent_state.arguments in
    node.persistent_state.arguments <- new_arguments
  ;;

  let make_full_address address port =
    let port_str = string_of_int port in
    address ^ ":" ^ port_str
  ;;

  let add_peer node peer =
    let address_str =
      Tezt.Runner.address
        ?from:node.N.persistent_state.runner
        peer.N.persistent_state.runner
    in
    let port = net_port peer in
    let address = make_full_address address_str port in
    add_argument node (Peer address)
  ;;

  let make_run_command node command args =
    let net_addr, rpc_addr =
      Option.fold
        ~none:("127.0.0.1", node.N.persistent_state.rpc_host)
        ~some:(fun _ -> "0.0.0.0", "0.0.0.0")
        node.N.persistent_state.runner
    in
    let args = node.N.persistent_state.arguments @ args in
    let cmd_a = make_arguments args in
    let cmd_a =
      Option.fold
        ~none:cmd_a
        ~some:(fun port ->
          let p = string_of_int port in
          "--advertised-net-port" :: p :: cmd_a)
        node.N.persistent_state.advertised_net_port
    in
    let net_port = node.N.persistent_state.net_port in
    let rpc_port = node.N.persistent_state.rpc_port in
    let net_addr_str = make_full_address net_addr net_port in
    let rpc_addr_str = make_full_address rpc_addr rpc_port in
    command
    :: "--data-dir"
    :: node.N.persistent_state.data_dir
    :: "--net-addr"
    :: net_addr_str
    :: "--rpc-addr"
    :: rpc_addr_str
    :: cmd_a
  ;;

  let do_run_command
      ?(on_terminate = fun _ -> ())
      ?event_level
      ?event_sections_levels
      node
      arguments
    =
    let () =
      match node.N.status with
      | N.Not_running -> ()
      | N.Running _ -> Tezt.Test.fail "node %s is already running" (name node)
    in
    let on_terminate status =
      let () = on_terminate status in
      let () = trigger_ready node None in
      let level = node.N.persistent_state.pending_level in
      let () = node.N.persistent_state.pending_level <- [] in
      let () = List.iter (fun (_, p) -> Lwt.wakeup_later p None) level in
      let idt = node.N.persistent_state.pending_identity in
      let () = node.N.persistent_state.pending_identity <- [] in
      let () = List.iter (fun p -> Lwt.wakeup_later p None) idt in
      Lwt.return_unit
    in
    N.run
      ?runner:node.N.persistent_state.runner
      ?event_level
      ?event_sections_levels
      ~on_terminate
      node
      { ready = false; level = None; identity = None }
      arguments
  ;;

  let run ?on_terminate ?event_level ?event_sections_levels node arguments =
    let arguments = make_run_command node "run" arguments in
    do_run_command
      ?on_terminate
      ?event_level
      ?event_sections_levels
      node
      arguments
  ;;

  let init
      ?runner
      ?path
      ?name
      ?color
      ?data_dir
      ?event_pipe
      ?net_port
      ?advertised_net_port
      ?rpc_host
      ?rpc_port
      ?event_level
      ?event_sections_levels
      arguments
    =
    let open Lwt.Syntax in
    let node =
      create
        ?runner
        ?path
        ?name
        ?color
        ?data_dir
        ?event_pipe
        ?net_port
        ?advertised_net_port
        ?rpc_host
        ?rpc_port
        arguments
    in
    let* () = identity_generate node in
    let* () = config_init node [] in
    let* () = run ?event_level ?event_sections_levels node [] in
    let* () = wait_for_ready node in
    Lwt.return node
  ;;

  module Config_file = struct
    let filename node = Format.asprintf "%s/config.json" (data_dir node)

    let read node =
      let config_file = filename node in
      Tezt.JSON.parse_file config_file
    ;;

    let write node config =
      let config_file = filename node in
      Tezt.JSON.encode_to_file config_file config
    ;;

    let update node json_updater = node |> read |> json_updater |> write node

    let sandbox_network_config =
      `O
        [ ( "genesis"
          , `O
              [ "timestamp", `String "2018-06-30T16:07:32Z"
              ; ( "block"
                , `String "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2"
                )
              ; "protocol", `String Protocol.genesis_hash
              ] )
        ; ( "genesis_parameters"
          , `O
              [ ( "values"
                , `O [ "genesis_pubkey", `String Key.activator.public_key ] )
              ] )
        ; "chain_name", `String "TEZOS"
        ; "sandboxed_chain_name", `String "SANDBOXED_TEZOS"
        ]
    ;;

    let set_sandbox_network_with_user_activated_upgrades
        upgrade_points
        old_config
      =
      let network =
        sandbox_network_config
        |> Tezt.JSON.annotate
             ~origin:"set_sandbox_network_with_user_activated_upgrades"
        |> Tezt.JSON.put
             ( "user_activated_upgrades"
             , Tezt.JSON.annotate ~origin:"user_activated_upgrades"
               @@ `A
                    (List.map
                       (fun (level, protocol) ->
                         `O
                           [ "level", `Float (float level)
                           ; ( "replacement_protocol"
                             , `String (Protocol.hash protocol) )
                           ])
                       upgrade_points) )
      in
      Tezt.JSON.put ("network", network) old_config
    ;;

    let set_sandbox_network_with_user_activated_overrides overrides old_config =
      let network =
        sandbox_network_config
        |> Tezt.JSON.annotate
             ~origin:"set_sandbox_network_with_user_activated_overrides"
        |> Tezt.JSON.put
             ( "user_activated_protocol_overrides"
             , Tezt.JSON.annotate ~origin:"user_activated_overrides"
               @@ `A
                    (List.map
                       (fun (replaced_protocol, replacement_protocol) ->
                         `O
                           [ "replaced_protocol", `String replaced_protocol
                           ; ( "replacement_protocol"
                             , `String replacement_protocol )
                           ])
                       overrides) )
      in
      Tezt.JSON.put ("network", network) old_config
    ;;

    let set_prevalidator
        ?(operations_request_timeout = 10.)
        ?(max_refused_operations = 1000)
        ?(operations_batch_size = 50)
        ?(disable_operations_precheck = false)
        old_config
      =
      let prevalidator =
        `O
          [ "operations_request_timeout", `Float operations_request_timeout
          ; ( "max_refused_operations"
            , `Float (float_of_int max_refused_operations) )
          ; "operations_batch_size", `Float (float_of_int operations_batch_size)
          ; "disable_precheck", `Bool disable_operations_precheck
          ]
        |> Tezt.JSON.annotate ~origin:"set_prevalidator"
      in
      Tezt.JSON.update
        "shell"
        (fun config -> Tezt.JSON.put ("prevalidator", prevalidator) config)
        old_config
    ;;

    let set_peer_validator ?(new_head_request_timeout = 60.) old_config =
      let peer_validator =
        `O
          [ ( "peer_validator"
            , `O [ "new_head_request_timeout", `Float new_head_request_timeout ]
            )
          ]
        |> Tezt.JSON.annotate ~origin:"set_peer_validator"
      in
      Tezt.JSON.put ("shell", peer_validator) old_config
    ;;
  end
end
