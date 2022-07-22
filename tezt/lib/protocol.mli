type t =
  | Ithaca
  | Jakarta
  | Alpha

type constants =
  | Constants_sandbox
  | Constants_mainnet
  | Constants_test

type supported_protocols =
  | Any_protocol
  | From_protocol of int
  | Until_protocol of int
  | Between_protocols of int * int

type parameter_overrides = (string list * string option) list

val default_constants : constants
val name : t -> string
val number : t -> int
val tag : t -> string
val hash : t -> string
val genesis_hash : string
val demo_counter_hash : string
val directory : t -> string
val parameter_file : ?constants:constants -> t -> string
val node_name : t -> string
val next_protocol : t -> t option
val previous_protocol : t -> t option
val all : t list

val write_parameter_file
  :  ?additional_bootstrap_accounts:(Key.t * int option) list
  -> base:(string, t * constants option) Either.t
  -> parameter_overrides
  -> string Lwt.t

val register_test
  :  __FILE__:string
  -> title:string
  -> tags:string list
  -> ?supports:supported_protocols
  -> (t -> unit Lwt.t)
  -> t list
  -> unit

val register_regression_test
  :  __FILE__:string
  -> title:string
  -> tags:string list
  -> ?supports:supported_protocols
  -> (t -> unit Lwt.t)
  -> t list
  -> unit
