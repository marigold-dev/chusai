module type CONFIG = sig
  type persistent_state
  type session_state

  val base_name : string
  val default_colors : Tezt.Log.Color.t array
end

module Level : sig
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

  val to_string : t -> string
end

module Make (C : CONFIG) () : sig
  module Session : sig
    type status =
      { process : Tezt.Process.t
      ; session_state : C.session_state
      ; mutable event_loop : unit Lwt.t option
      }
  end

  module Event : sig
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

  val terminate : ?kill:bool -> t -> unit Lwt.t
  val fresh_name : unit -> string
  val next_color : unit -> Tezt.Log.Color.t
  val get_event_from_full_event : Tezt.JSON.t -> Event.t option

  val create
    :  path:string
    -> ?runner:Tezt.Runner.t
    -> ?name:string
    -> ?color:Tezt.Log.Color.t
    -> ?event_pipe:string
    -> C.persistent_state
    -> t

  val run
    :  ?runner:Tezt.Runner.t
    -> ?on_terminate:(Unix.process_status -> unit Lwt.t)
    -> ?event_level:Level.default
    -> ?event_sections_levels:(string * Level.t) list
    -> t
    -> C.session_state
    -> string list
    -> unit Lwt.t

  val wait_for_full
    :  ?where:string
    -> t
    -> string
    -> (Tezt.JSON.t -> 'a option)
    -> 'a Lwt.t

  val wait_for : ?where:string -> t -> string -> (Tezt.JSON.t -> 'a option) -> 'a Lwt.t
  val on_event : t -> (Event.t -> unit) -> unit
  val on_stdout : t -> (string -> unit) -> unit
  val log_events : t -> unit
end

module Tezos : sig
  type history_mode =
    | Archive
    | Full of int option
    | Rolling of int option

  type media_type =
    | Json
    | Binary
    | Any

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

  type t
  type event

  val create
    :  ?runner:Tezt.Runner.t
    -> ?path:string
    -> ?name:string
    -> ?color:Tezt.Log.Color.t
    -> ?data_dir:string
    -> ?event_pipe:string
    -> ?net_port:int
    -> ?advertised_net_port:int
    -> ?rpc_host:string
    -> ?rpc_port:int
    -> argument list
    -> t

  val init
    :  ?runner:Tezt.Runner.t
    -> ?path:string
    -> ?name:string
    -> ?color:Tezt.Log.Color.t
    -> ?data_dir:string
    -> ?event_pipe:string
    -> ?net_port:int
    -> ?advertised_net_port:int
    -> ?rpc_host:string
    -> ?rpc_port:int
    -> ?event_level:Level.default
    -> ?event_sections_levels:(string * Level.t) list
    -> argument list
    -> t Lwt.t

  val run
    :  ?on_terminate:(Unix.process_status -> unit)
    -> ?event_level:Level.default
    -> ?event_sections_levels:(string * Level.t) list
    -> t
    -> argument list
    -> unit Lwt.t

  val add_argument : t -> argument -> unit
  val add_peer : t -> t -> unit
  val name : t -> string
  val net_port : t -> int
  val advertised_net_port : t -> int option
  val rpc_host : t -> string
  val rpc_port : t -> int
  val data_dir : t -> string
  val runner : t -> Tezt.Runner.t option
  val check_error : ?exit_code:int -> ?msg:Tezt.Base.rex -> t -> unit Lwt.t
  val wait : t -> Unix.process_status Lwt.t
  val terminate : ?kill:bool -> t -> unit Lwt.t
  val identity_generate : ?expected_pow:int -> t -> unit Lwt.t
  val spawn_identity_generate : ?expected_pow:int -> t -> Tezt.Process.t
  val config_init : t -> argument list -> unit Lwt.t
  val wait_for_ready : t -> unit Lwt.t
  val wait_for_level : t -> int -> int Lwt.t
  val get_level : t -> int
  val wait_for_identity : t -> string Lwt.t

  val wait_for_request
    :  request:[< `Flush | `Inject | `Notify | `Arrived ]
    -> t
    -> unit Lwt.t

  val wait_for_full
    :  ?where:string
    -> t
    -> string
    -> (Tezt.JSON.t -> 'a option)
    -> 'a Lwt.t

  val wait_for : ?where:string -> t -> string -> (Tezt.JSON.t -> 'a option) -> 'a Lwt.t
  val on_event : t -> (event -> unit) -> unit
  val on_stdout : t -> (string -> unit) -> unit
  val log_events : t -> unit

  module Config_file : sig
    val filename : t -> string
    val read : t -> Tezt.JSON.t
    val write : t -> Tezt.JSON.t -> unit
    val update : t -> (Tezt.JSON.t -> Tezt.JSON.t) -> unit

    val set_sandbox_network_with_user_activated_upgrades
      :  (int * Protocol.t) list
      -> Tezt.JSON.t
      -> Tezt.JSON.t

    val set_sandbox_network_with_user_activated_overrides
      :  (string * string) list
      -> Tezt.JSON.t
      -> Tezt.JSON.t

    val set_prevalidator
      :  ?operations_request_timeout:float
      -> ?max_refused_operations:int
      -> ?operations_batch_size:int
      -> ?disable_operations_precheck:bool
      -> Tezt.JSON.t
      -> Tezt.JSON.t

    val set_peer_validator : ?new_head_request_timeout:float -> Tezt.JSON.t -> Tezt.JSON.t
  end
end
