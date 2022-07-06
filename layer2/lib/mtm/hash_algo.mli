module type HASH = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val empty : t
  val hash : 'a -> t
  val ( ++ ) : t -> t -> t
end
