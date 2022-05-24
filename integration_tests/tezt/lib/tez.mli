type t

val of_int : int -> t
val of_mutez_int : int -> t
val zero : t
val one : t
val to_string : t -> string
val mutez_int64 : t -> int64
val to_float : t -> float
val to_mutez : t -> int
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val parse_floating : string -> t
