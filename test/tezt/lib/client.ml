type t =
  { path : string
  ; name : string
  ; color : Tezt.Log.Color.t
  ; base_dir : string
  ; mutable additional_bootstraps : Key.t list
  ; mutable node : Node.Tezos.t option * media_type option
  }

and media_type =
  | Json
  | Binary
  | Any

type timestamp =
  | Now
  | Ago of Ptime.Span.t
  | At of Ptime.t

let next_name = ref 1

let fresh_name () =
  let index = !next_name in
  let () = incr next_name in
  let str_index = string_of_int index in
  "client" ^ str_index
;;

let () =
  let callback () = next_name := 1 in
  Tezt.Test.declare_reset_function callback
;;

let name t = t.name
let base_dir t = t.base_dir
let additional_bootstraps t = t.additional_bootstraps
let runner node = Node.Tezos.runner node

let address ?(hostname = false) ?from peer =
  match from with
  | None -> Tezt.Runner.address ~hostname (runner peer)
  | Some endpoint ->
    Tezt.Runner.address ~hostname ?from:(runner endpoint) (runner peer)
;;

let create_with
    ?(path = Constants.tezos_client)
    ?name
    ?(color = Tezt.Log.Color.FG.blue)
    ?base_dir
    node_with_media
  =
  let name =
    match name with
    | None -> fresh_name ()
    | Some name -> name
  in
  let base_dir =
    match base_dir with
    | None -> Tezt.Temp.dir name
    | Some dir -> dir
  in
  { path
  ; name
  ; color
  ; base_dir
  ; additional_bootstraps = []
  ; node = node_with_media
  }
;;

let create ?path ?name ?color ?base_dir ?node ?media_type () =
  create_with ?path ?name ?color ?base_dir (node, media_type)
;;

let endpoint_arg ?node client =
  let first_some o1 o2 =
    match o1, o2 with
    | Some _, _ -> o1
    | _ -> o2
  in
  match first_some node (fst client.node) with
  | None -> []
  | Some node ->
    let address = address ~hostname:true node in
    let rpc_port = Node.Tezos.rpc_port node in
    [ "--endpoint"; Format.asprintf "http://%s:%d" address rpc_port ]
;;

let media_type_arg client =
  match client with
  | _, Some media_type ->
    (match media_type with
    | Json -> [ "--media-type"; "json" ]
    | Binary -> [ "--media-type"; "binary" ]
    | Any -> [ "--media-type"; "any" ])
  | _ -> []
;;

let mode_arg _ = []
let base_dir_arg client = [ "--base-dir"; client.base_dir ]

let spawn_command
    ?log_command
    ?log_status_on_exit
    ?log_output
    ?(env = Tezt.Base.String_map.empty)
    ?node
    ?hooks
    client
    command
  =
  let env =
    Tezt.Base.String_map.update
      "TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER"
      (fun o -> Option.value ~default:"Y" o |> Option.some)
      env
  in
  Tezt.Process.spawn
    ~name:client.name
    ~color:client.color
    ~env
    ?log_command
    ?log_status_on_exit
    ?log_output
    ?hooks
    client.path
  @@ endpoint_arg ?node client
  @ media_type_arg client.node
  @ mode_arg client
  @ base_dir_arg client
  @ command
;;

let spawn_rpc_aux
    ?log_command
    ?log_status_on_exit
    ?log_output
    ?(better_errors = false)
    ?node
    ?hooks
    ?env
    ?data
    ?query_string
    meth
    path
    client
  =
  let process =
    let data =
      Option.fold
        ~none:[]
        ~some:(fun x -> [ "with"; Tezt.JSON.encode_u x ])
        data
    in
    let query_string =
      Option.fold ~none:"" ~some:Url.query_to_string query_string
    in
    let path = Url.path_to_string path in
    let full_path = path ^ query_string in
    let better_error = if better_errors then [ "--better-errors" ] else [] in
    spawn_command
      ?log_command
      ?log_status_on_exit
      ?log_output
      ?node
      ?hooks
      ?env
      client
      (better_error @ [ "rpc"; Url.method_to_string meth; full_path ] @ data)
  in
  let parse process =
    let open Lwt.Syntax in
    let* output = Tezt.Process.check_and_read_stdout process in
    Lwt.return
      (Tezt.JSON.parse ~origin:(Url.path_to_string path ^ " response") output)
  in
  Runnable.{ value = process; run = parse }
;;

let spawn_rpc
    ?log_command
    ?log_status_on_exit
    ?log_output
    ?better_errors
    ?node
    ?hooks
    ?env
    ?data
    ?query_string
    meth
    path
    client
  =
  let open Runnable.Syntax in
  let*? res =
    spawn_rpc_aux
      ?log_command
      ?log_status_on_exit
      ?log_output
      ?better_errors
      ?node
      ?hooks
      ?env
      ?data
      ?query_string
      meth
      path
      client
  in
  res
;;

let rpc
    ?log_command
    ?log_status_on_exit
    ?log_output
    ?better_errors
    ?node
    ?hooks
    ?env
    ?data
    ?query_string
    meth
    path
    client
  =
  let open Runnable.Syntax in
  let*! res =
    spawn_rpc_aux
      ?log_command
      ?log_status_on_exit
      ?log_output
      ?better_errors
      ?node
      ?hooks
      ?env
      ?data
      ?query_string
      meth
      path
      client
  in
  Lwt.return res
;;

let spawn_rpc_list ?node client = spawn_command ?node client [ "rpc"; "list" ]

let rpc_list ?node client =
  spawn_rpc_list ?node client |> Tezt.Process.check_and_read_stdout
;;

let spawn_shell_header ?node ?(chain = "main") ?(block = "head") client =
  let path = [ "chains"; chain; "blocks"; block; "header"; "shell" ] in
  spawn_rpc ?node GET path client
;;

let shell_header ?node ?chain ?block client =
  spawn_shell_header ?node ?chain ?block client
  |> Tezt.Process.check_and_read_stdout
;;

let level ?node ?chain ?block client =
  let open Lwt.Syntax in
  let* shell = shell_header ?node ?chain ?block client in
  let json = Tezt.JSON.parse ~origin:"level" shell in
  Tezt.JSON.get "level" json |> Tezt.JSON.as_int |> Lwt.return
;;

let spawn_version client = spawn_command client [ "--version" ]
let version client = spawn_version client |> Tezt.Process.check

let spawn_import_secret_key ?node client key =
  let sk_uri =
    let sk = key.Key.secret_key in
    "unencrypted:" ^ sk
  in
  spawn_command ?node client [ "import"; "secret"; "key"; key.alias; sk_uri ]
;;

let spawn_import_signer_key ?node ?(force = false) client key signer_uri =
  let uri = Uri.with_path signer_uri key.Key.public_key_hash in
  spawn_command
    ?node
    client
    ([ "import"; "secret"; "key"; key.alias; Uri.to_string uri ]
    @ if force then [ "--force" ] else [])
;;

let import_signer_key ?node ?force client key signer_uri =
  spawn_import_signer_key ?node ?force client key signer_uri
  |> Tezt.Process.check
;;

let import_secret_key ?node client key =
  spawn_import_secret_key ?node client key |> Tezt.Process.check
;;

let default_delay = Ptime.Span.of_float_s (3600. *. 24. *. 365.) |> Option.get

let time_of_timestamp timestamp =
  match timestamp with
  | Now -> Ptime_clock.now ()
  | Ago delay ->
    (match Ptime.sub_span (Ptime_clock.now ()) delay with
    | None -> Ptime.epoch
    | Some tm -> tm)
  | At tm -> tm
;;

let spawn_activate_protocol
    ?node
    ~protocol
    ?(fitness = 1)
    ?(key = Key.activator.alias)
    ?(timestamp = Ago default_delay)
    ?parameter_file
    client
  =
  let timestamp = time_of_timestamp timestamp in
  spawn_command
    ?node
    client
    [ "activate"
    ; "protocol"
    ; Protocol.hash protocol
    ; "with"
    ; "fitness"
    ; string_of_int fitness
    ; "and"
    ; "key"
    ; key
    ; "and"
    ; "parameters"
    ; Option.value parameter_file ~default:(Protocol.parameter_file protocol)
    ; "--timestamp"
    ; Ptime.to_rfc3339 ~frac_s:3 timestamp
    ]
;;

let activate_protocol
    ?node
    ~protocol
    ?fitness
    ?key
    ?timestamp
    ?parameter_file
    client
  =
  spawn_activate_protocol
    ?node
    ~protocol
    ?fitness
    ?key
    ?timestamp
    ?parameter_file
    client
  |> Tezt.Process.check
;;

let empty_mempool_file ?(filename = "mempool.json") () =
  let mempool_str = "[]" in
  let mempool = Tezt.Temp.file filename in
  Tezt.Base.write_file mempool ~contents:mempool_str;
  mempool
;;

let optional_arg ~name f = function
  | None -> []
  | Some x -> [ "--" ^ name; f x ]
;;

let spawn_bake_for
    ?node
    ?protocol
    ?(keys = [ Key.sample.(0).alias ])
    ?minimal_fees
    ?minimal_nanotez_per_gas_unit
    ?minimal_nanotez_per_byte
    ?(minimal_timestamp = true)
    ?mempool
    ?(ignore_node_mempool = false)
    ?force
    ?context_path
    client
  =
  spawn_command
    ?node
    client
    (optional_arg ~name:"protocol" Protocol.hash protocol
    @ [ "bake"; "for" ]
    @ keys
    @ optional_arg ~name:"minimal-fees" string_of_int minimal_fees
    @ optional_arg
        ~name:"minimal-nanotez-per-gas-unit"
        string_of_int
        minimal_nanotez_per_gas_unit
    @ optional_arg
        ~name:"minimal-nanotez-per-byte"
        string_of_int
        minimal_nanotez_per_byte
    @ optional_arg ~name:"operations-pool" (fun x -> x) mempool
    @ (if ignore_node_mempool then [ "--ignore-node-mempool" ] else [])
    @ (if minimal_timestamp then [ "--minimal-timestamp" ] else [])
    @ (match force with
      | None | Some false -> []
      | Some true -> [ "--force" ])
    @ optional_arg ~name:"context" (fun x -> x) context_path)
;;

let bake_for
    ?node
    ?protocol
    ?keys
    ?minimal_fees
    ?minimal_nanotez_per_gas_unit
    ?minimal_nanotez_per_byte
    ?minimal_timestamp
    ?mempool
    ?ignore_node_mempool
    ?force
    ?context_path
    client
  =
  spawn_bake_for
    ?node
    ?keys
    ?minimal_fees
    ?minimal_nanotez_per_gas_unit
    ?minimal_nanotez_per_byte
    ?minimal_timestamp
    ?mempool
    ?ignore_node_mempool
    ?force
    ?context_path
    ?protocol
    client
  |> Tezt.Process.check
;;

let bake_for_and_wait
    ?protocol
    ?keys
    ?minimal_fees
    ?minimal_nanotez_per_gas_unit
    ?minimal_nanotez_per_byte
    ?minimal_timestamp
    ?mempool
    ?ignore_node_mempool
    ?force
    ?context_path
    ?node
    client
  =
  let node =
    match node with
    | Some n -> n
    | None ->
      (match fst client.node with
      | Some n -> n
      | None -> Tezt.Test.fail "No node found for bake_for_and_wait")
  in
  let level_before = Node.Tezos.get_level node in
  let open Lwt.Syntax in
  let* () =
    bake_for
      ~node
      ?protocol
      ?keys
      ?minimal_fees
      ?minimal_nanotez_per_gas_unit
      ?minimal_nanotez_per_byte
      ?minimal_timestamp
      ?mempool
      ?ignore_node_mempool
      ?force
      ?context_path
      client
  in
  let* _lvl = Node.Tezos.wait_for_level node (level_before + 1) in
  Lwt.return_unit
;;

let spawn_transfer
    ?hooks
    ?log_output
    ?node
    ?(wait = "none")
    ?burn_cap
    ?fee
    ?gas_limit
    ?storage_limit
    ?counter
    ?arg
    ?(simulation = false)
    ?(force = false)
    ~amount
    ~giver
    ~receiver
    client
  =
  spawn_command
    ?log_output
    ?node
    ?hooks
    client
    ([ "--wait"; wait ]
    @ [ "transfer"; Tez.to_string amount; "from"; giver; "to"; receiver ]
    @ Option.fold
        ~none:[]
        ~some:(fun f -> [ "--fee"; Tez.to_string f; "--force-low-fee" ])
        fee
    @ optional_arg ~name:"burn-cap" Tez.to_string burn_cap
    @ optional_arg ~name:"gas-limit" string_of_int gas_limit
    @ optional_arg ~name:"storage-limit" string_of_int storage_limit
    @ optional_arg ~name:"counter" string_of_int counter
    @ optional_arg ~name:"arg" (fun x -> x) arg
    @ (if simulation then [ "--simulation" ] else [])
    @ if force then [ "--force" ] else [])
;;

let transfer
    ?hooks
    ?log_output
    ?node
    ?wait
    ?burn_cap
    ?fee
    ?gas_limit
    ?storage_limit
    ?counter
    ?arg
    ?simulation
    ?force
    ?expect_failure
    ~amount
    ~giver
    ~receiver
    client
  =
  spawn_transfer
    ?log_output
    ?node
    ?hooks
    ?wait
    ?burn_cap
    ?fee
    ?gas_limit
    ?storage_limit
    ?counter
    ?arg
    ?simulation
    ?force
    ~amount
    ~giver
    ~receiver
    client
  |> Tezt.Process.check ?expect_failure
;;

let spawn_originate_contract
    ?hooks
    ?log_output
    ?node
    ?(wait = "none")
    ?init
    ?burn_cap
    ~alias
    ~amount
    ~src
    ~prg
    client
  =
  spawn_command
    ?hooks
    ?log_output
    ?node
    client
    ([ "--wait"; wait ]
    @ [ "originate"
      ; "contract"
      ; alias
      ; "transferring"
      ; Tez.to_string amount
      ; "from"
      ; src
      ; "running"
      ; prg
      ]
    @ optional_arg ~name:"init" Fun.id init
    @ optional_arg ~name:"burn-cap" Tez.to_string burn_cap)
;;

let originate_contract
    ?hooks
    ?log_output
    ?node
    ?wait
    ?init
    ?burn_cap
    ~alias
    ~amount
    ~src
    ~prg
    client
  =
  let open Lwt.Syntax in
  let* client_output =
    spawn_originate_contract
      ?node
      ?log_output
      ?hooks
      ?wait
      ?init
      ?burn_cap
      ~alias
      ~amount
      ~src
      ~prg
      client
    |> Tezt.Process.check_and_read_stdout
  in
  match Tezt.Base.(client_output =~* rex "New contract ?(KT1\\w{33})") with
  | None ->
    Tezt.Test.fail
      "Cannot extract contract hash from client_output: %s"
      client_output
  | Some hash -> Lwt.return hash
;;

let init_with_node
    ?path
    ?name
    ?color
    ?base_dir
    ?event_level
    ?event_sections_levels
    ?(nodes_args = Node.Tezos.[ Connections 0; Synchronisation_threshold 0 ])
    ?(keys = Key.all)
    ()
  =
  let open Lwt.Syntax in
  let* node = Node.Tezos.init ?event_level ?event_sections_levels nodes_args in
  let mode = Some node, None in
  let client = create_with ?path ?name ?color ?base_dir mode in
  Key.write keys ~directory:client.base_dir;
  Lwt.return (node, client)
;;

let stresstest_gen_keys ?node n client =
  let alias n = "bootstrap" ^ string_of_int n in
  let open Lwt.Syntax in
  let* output =
    spawn_command ?node client [ "stresstest"; "gen"; "keys"; Int.to_string n ]
    |> Tezt.Process.check_and_read_stdout
  in
  let json = Tezt.JSON.parse ~origin:"stresstest_gen_keys" output in
  let read_one i json =
    let bootstrap_accounts = Key.sample |> Array.length in
    let alias = alias (i + bootstrap_accounts + 1) in
    let public_key_hash = Tezt.JSON.(json |-> "pkh" |> as_string) in
    let public_key = Tezt.JSON.(json |-> "pk" |> as_string) in
    let secret_key = Tezt.JSON.(json |-> "sk" |> as_string) in
    Key.{ alias; public_key_hash; public_key; secret_key }
  in
  let additional_bootstraps = List.mapi read_one (Tezt.JSON.as_list json) in
  client.additional_bootstraps <- additional_bootstraps;
  Lwt.return additional_bootstraps
;;

let get_parameter_file
    ?additional_bootstrap_accounts
    ?default_accounts_balance
    ?parameter_file
    protocol
  =
  let open Lwt.Syntax in
  match additional_bootstrap_accounts with
  | None -> Lwt.return parameter_file
  | Some additional_account_keys ->
    let additional_bootstraps =
      List.map (fun x -> x, default_accounts_balance) additional_account_keys
    in
    let* parameter_file =
      Protocol.write_parameter_file
        ~additional_bootstrap_accounts:additional_bootstraps
        ~base:
          (Option.fold
             ~none:(Either.right protocol)
             ~some:Either.left
             parameter_file)
        []
    in
    Lwt.return (Some parameter_file)
;;

let init_with_protocol
    ?path
    ?name
    ?color
    ?base_dir
    ?event_level
    ?event_sections_levels
    ?nodes_args
    ?additional_bootstrap_account_count
    ?default_accounts_balance
    ?parameter_file
    ?timestamp
    ?keys
    ~protocol
    ()
  =
  let open Lwt.Syntax in
  let* node, client =
    init_with_node
      ?path
      ?name
      ?color
      ?base_dir
      ?event_level
      ?event_sections_levels
      ?nodes_args
      ?keys
      ()
  in
  let* additional_bootstrap_accounts =
    match additional_bootstrap_account_count with
    | None -> Lwt.return None
    | Some n ->
      let* r = stresstest_gen_keys n client in
      Lwt.return (Some r)
  in
  let* parameter_file =
    get_parameter_file
      ?additional_bootstrap_accounts
      ?default_accounts_balance
      ?parameter_file
      (protocol, None)
  in
  let* () = activate_protocol ?parameter_file ~protocol ?timestamp client in
  let* _ = Node.Tezos.wait_for_level node 1 in
  Lwt.return (node, client)
;;
