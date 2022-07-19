type method_ =
  | GET
  | PUT
  | POST
  | PATCH
  | DELETE

val encode : string -> string
val method_to_string : method_ -> string
val path_to_string : string list -> string
val query_to_string : (string * string) list -> string
val rpc_path : ?query_string:(string * string) list -> string list -> string
