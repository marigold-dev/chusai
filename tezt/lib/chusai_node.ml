open Tezos_base.TzPervasives
open Tezt

module Config = struct
  type persistent_state =
    { tezos_node : Node.Tezos.t
    ; client : Client.t
    ; runner : Runner.t option
    ; operator : string
    ; inbox_address : string
    ; rpc_address : string
    ; rpc_port : int
    ; mutable pending_ready : unit option Lwt.u list
    }

  type session_state = { mutable ready : bool }

  let base_name = "chusai-node"
  let default_colors = Log.Color.[| FG.green; FG.magenta; FG.yellow; FG.cyan |]
end

open Config
include Node.Make (Config) ()

let rpc_address node = node.persistent_state.rpc_address
let rpc_port node = node.persistent_state.rpc_port
let endpoint node = Format.sprintf "%s:%d" (rpc_address node) (rpc_port node)
let operator node = node.persistent_state.operator
let inbox_address node = node.persistent_state.inbox_address
let spawn_command node = Process.spawn ~name:node.name ~color:node.color node.path

let check_event ?where node name promise =
  let open Lwt.Syntax in
  let* result = promise in
  match result with
  | None -> raise (Event.Terminated_before { node = node.name; event = name; where })
  | Some x -> Lwt.return x
;;

let trigger_ready node value =
  let pending = node.persistent_state.pending_ready in
  let () = node.persistent_state.pending_ready <- [] in
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending
;;

let set_ready node =
  let () =
    match node.status with
    | Not_running -> ()
    | Running status -> status.session_state.ready <- true
  in
  trigger_ready node (Some ())
;;

let handle_event node Event.{ name; _ } =
  match name with
  | "chusai_node_is_ready.v0" -> set_ready node
  | _ -> ()
;;

let wait_for_ready node =
  match node.status with
  | Running { session_state = { ready = true; _ }; _ } -> Lwt.return_unit
  | Not_running | Running { session_state = { ready = false; _ }; _ } ->
    let promise, resolver = Lwt.task () in
    let () =
      node.persistent_state.pending_ready
        <- resolver :: node.persistent_state.pending_ready
    in
    check_event node "chusai_node_is_ready.v0" promise
;;

let create
    ?(path = Constants.chusai_node)
    ?runner
    ?name
    ?color
    ?event_pipe
    ?(rpc_address = "127.0.0.1")
    ?rpc_port
    ~operator
    ~inbox_address
    client
    tezos_node
  =
  let name =
    match name with
    | None -> fresh_name ()
    | Some name -> name
  in
  let rpc_port =
    match rpc_port with
    | None -> Port.fresh ()
    | Some port -> port
  in
  let chusai_node =
    create
      ?runner
      ~path
      ~name
      ?color
      ?event_pipe
      { tezos_node
      ; client
      ; inbox_address
      ; runner
      ; operator = operator.Key.public_key_hash
      ; rpc_port
      ; rpc_address
      ; pending_ready = []
      }
  in
  let () = on_event chusai_node (handle_event chusai_node) in
  chusai_node
;;

let make_tezos_node_arguments node =
  let tezos_node = node.persistent_state.tezos_node in
  [ "--endpoint"
  ; Format.sprintf
      "http://%s:%d"
      (Node.Tezos.rpc_host tezos_node)
      (Node.Tezos.rpc_port tezos_node)
  ]
;;

let do_runlike_command node arguments =
  let () =
    if node.status <> Not_running
    then Test.fail "Chusai node %s is already running" node.name
  in
  let on_terminate _ =
    let () = trigger_ready node None in
    Lwt.return_unit
  in
  let arguments = arguments @ make_tezos_node_arguments node in
  run node { ready = false } arguments ~on_terminate
;;

let run node =
  do_runlike_command
    node
    [ "run"
    ; "--rpc-address"
    ; rpc_address node
    ; "--rpc-port"
    ; string_of_int @@ rpc_port node
    ; "--operator"
    ; operator node
    ; "--inbox"
    ; inbox_address node
    ]
;;

let rpc ~service node =
  let open Lwt_syntax in
  let* opt_get = Rpc.Curl.get () in
  match opt_get with
  | None -> return None
  | Some get ->
    let url = Format.asprintf "%s/%s" (endpoint node) service in
    let* result = get ~url in
    let parsed = JSON.parse ~origin:service result in
    return_some parsed
;;
