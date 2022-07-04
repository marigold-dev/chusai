open Format

module type Hash = sig
  type t 

  val equal : t -> t -> bool
  val pp : formatter -> t -> unit

  val empty : t
  val hash : 'a -> t
  val (++) : t -> t -> t
end