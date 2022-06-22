open Format

module type Hash = sig
  type t 

  val equal : t -> t -> bool
  val pp : formatter -> t -> unit

  val empty : t
  val hash : 'a -> t
  val (++) : t -> t -> t

  (* For debugging purposes. Please remove before merge!!!!*)
  val unsafe_from_string : string -> t
end