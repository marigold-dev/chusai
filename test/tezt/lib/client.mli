type t

type media_type =
  | Json
  | Binary
  | Any

type timestamp =
  | Now
  | Ago of Ptime.Span.t
  | At of Ptime.t

val name : t -> string
val base_dir : t -> string
val additional_bootstraps : t -> Key.t list

val create_with
  :  ?path:string
  -> ?name:string
  -> ?color:Tezt.Log.Color.t
  -> ?base_dir:string
  -> Node.Tezos.t option * media_type option
  -> t

val create
  :  ?path:string
  -> ?name:string
  -> ?color:Tezt.Log.Color.t
  -> ?base_dir:string
  -> ?node:Node.Tezos.t
  -> ?media_type:media_type
  -> unit
  -> t

val spawn_command
  :  ?log_command:bool
  -> ?log_status_on_exit:bool
  -> ?log_output:bool
  -> ?env:string Tezt.Base.String_map.t
  -> ?node:Node.Tezos.t
  -> ?hooks:Tezt.Process.hooks
  -> t
  -> string list
  -> Tezt.Process.t

val spawn_rpc
  :  ?log_command:bool
  -> ?log_status_on_exit:bool
  -> ?log_output:bool
  -> ?better_errors:bool
  -> ?node:Node.Tezos.t
  -> ?hooks:Tezt.Process.hooks
  -> ?env:string Tezt.Base.String_map.t
  -> ?data:Tezt.JSON.u
  -> ?query_string:(string * string) list
  -> Url.method_
  -> string list
  -> t
  -> Tezt.Process.t

val rpc
  :  ?log_command:bool
  -> ?log_status_on_exit:bool
  -> ?log_output:bool
  -> ?better_errors:bool
  -> ?node:Node.Tezos.t
  -> ?hooks:Tezt.Process.hooks
  -> ?env:string Tezt.Base.String_map.t
  -> ?data:Tezt.JSON.u
  -> ?query_string:(string * string) list
  -> Url.method_
  -> string list
  -> t
  -> Tezt.JSON.t Lwt.t

val rpc_list : ?node:Node.Tezos.t -> t -> string Lwt.t
val spawn_rpc_list : ?node:Node.Tezos.t -> t -> Tezt.Process.t

val shell_header
  :  ?node:Node.Tezos.t
  -> ?chain:string
  -> ?block:string
  -> t
  -> string Lwt.t

val spawn_shell_header
  :  ?node:Node.Tezos.t
  -> ?chain:string
  -> ?block:string
  -> t
  -> Tezt.Process.t

val level
  :  ?node:Node.Tezos.t
  -> ?chain:string
  -> ?block:string
  -> t
  -> int Lwt.t

val version : t -> unit Lwt.t
val spawn_version : t -> Tezt.Process.t
val import_secret_key : ?node:Node.Tezos.t -> t -> Key.t -> unit Lwt.t

val import_signer_key
  :  ?node:Node.Tezos.t
  -> ?force:bool
  -> t
  -> Key.t
  -> Uri.t
  -> unit Lwt.t

val spawn_import_signer_key
  :  ?node:Node.Tezos.t
  -> ?force:bool
  -> t
  -> Key.t
  -> Uri.t
  -> Tezt.Process.t

val spawn_import_secret_key : ?node:Node.Tezos.t -> t -> Key.t -> Tezt.Process.t

val activate_protocol
  :  ?node:Node.Tezos.t
  -> protocol:Protocol.t
  -> ?fitness:int
  -> ?key:string
  -> ?timestamp:timestamp
  -> ?parameter_file:string
  -> t
  -> unit Lwt.t

val spawn_activate_protocol
  :  ?node:Node.Tezos.t
  -> protocol:Protocol.t
  -> ?fitness:int
  -> ?key:string
  -> ?timestamp:timestamp
  -> ?parameter_file:string
  -> t
  -> Tezt.Process.t

val empty_mempool_file : ?filename:string -> unit -> string

val bake_for
  :  ?node:Node.Tezos.t
  -> ?protocol:Protocol.t
  -> ?keys:string list
  -> ?minimal_fees:int
  -> ?minimal_nanotez_per_gas_unit:int
  -> ?minimal_nanotez_per_byte:int
  -> ?minimal_timestamp:bool
  -> ?mempool:string
  -> ?ignore_node_mempool:bool
  -> ?force:bool
  -> ?context_path:string
  -> t
  -> unit Lwt.t

val bake_for_and_wait
  :  ?protocol:Protocol.t
  -> ?keys:string list
  -> ?minimal_fees:int
  -> ?minimal_nanotez_per_gas_unit:int
  -> ?minimal_nanotez_per_byte:int
  -> ?minimal_timestamp:bool
  -> ?mempool:string
  -> ?ignore_node_mempool:bool
  -> ?force:bool
  -> ?context_path:string
  -> ?node:Node.Tezos.t
  -> t
  -> unit Lwt.t

val spawn_bake_for
  :  ?node:Node.Tezos.t
  -> ?protocol:Protocol.t
  -> ?keys:string list
  -> ?minimal_fees:int
  -> ?minimal_nanotez_per_gas_unit:int
  -> ?minimal_nanotez_per_byte:int
  -> ?minimal_timestamp:bool
  -> ?mempool:string
  -> ?ignore_node_mempool:bool
  -> ?force:bool
  -> ?context_path:string
  -> t
  -> Tezt.Process.t

val transfer
  :  ?hooks:Tezt.Process.hooks
  -> ?log_output:bool
  -> ?node:Node.Tezos.t
  -> ?wait:string
  -> ?burn_cap:Tez.t
  -> ?fee:Tez.t
  -> ?gas_limit:int
  -> ?storage_limit:int
  -> ?counter:int
  -> ?arg:string
  -> ?simulation:bool
  -> ?force:bool
  -> ?expect_failure:bool
  -> amount:Tez.t
  -> giver:string
  -> receiver:string
  -> t
  -> unit Lwt.t

val spawn_transfer
  :  ?hooks:Tezt.Process.hooks
  -> ?log_output:bool
  -> ?node:Node.Tezos.t
  -> ?wait:string
  -> ?burn_cap:Tez.t
  -> ?fee:Tez.t
  -> ?gas_limit:int
  -> ?storage_limit:int
  -> ?counter:int
  -> ?arg:string
  -> ?simulation:bool
  -> ?force:bool
  -> amount:Tez.t
  -> giver:string
  -> receiver:string
  -> t
  -> Tezt.Process.t

val originate_contract
  :  ?hooks:Tezt.Process.hooks
  -> ?log_output:bool
  -> ?node:Node.Tezos.t
  -> ?wait:string
  -> ?init:string
  -> ?burn_cap:Tez.t
  -> alias:string
  -> amount:Tez.t
  -> src:string
  -> prg:string
  -> t
  -> string Lwt.t

val spawn_originate_contract
  :  ?hooks:Tezt.Process.hooks
  -> ?log_output:bool
  -> ?node:Node.Tezos.t
  -> ?wait:string
  -> ?init:string
  -> ?burn_cap:Tez.t
  -> alias:string
  -> amount:Tez.t
  -> src:string
  -> prg:string
  -> t
  -> Tezt.Process.t

val init_with_node
  :  ?path:string
  -> ?name:string
  -> ?color:Tezt.Log.Color.t
  -> ?base_dir:string
  -> ?event_level:Node.Level.default
  -> ?event_sections_levels:(string * Node.Level.t) list
  -> ?nodes_args:Node.Tezos.argument list
  -> ?keys:Key.t list
  -> unit
  -> (Node.Tezos.t * t) Lwt.t

val init_with_protocol
  :  ?path:string
  -> ?name:string
  -> ?color:Tezt.Log.Color.t
  -> ?base_dir:string
  -> ?event_level:Node.Level.default
  -> ?event_sections_levels:(string * Node.Level.t) list
  -> ?nodes_args:Node.Tezos.argument list
  -> ?additional_bootstrap_account_count:int
  -> ?default_accounts_balance:int
  -> ?parameter_file:string
  -> ?timestamp:timestamp
  -> ?keys:Key.t list
  -> protocol:Protocol.t
  -> unit
  -> (Node.Tezos.t * t) Lwt.t
