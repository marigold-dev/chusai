module type SERIALIZER = sig
  val serialize : 'a -> bytes
end

module type HASH = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val empty : t
  val hash : 'a -> t
  val ( ++ ) : t -> t -> t
end

module type MERKLEMAP = sig
  type hash
  type ('k, 'v) t
  type 'k proof
  type ('k, 'v) op

  val empty : ('k, 'v) t
  val from_list : ('k * 'v) list -> ('k, 'v) t
  val to_list : ('k, 'v) t -> ('k * 'v) list
  val lookup : 'k -> ('k, 'v) t -> ('k, 'v) op * 'k proof * 'v option

  val update_map
    :  'k
    -> ('v option -> 'v option)
    -> ('k, 'v) t
    -> ('k, 'v) op * 'k proof * ('k, 'v) t

  val upsert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) op * 'k proof * ('k, 'v) t
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) op * 'k proof * ('k, 'v) t
  val root_hash : ('k, 'v) t -> hash
  val verify_proof : ('k, 'v) op -> 'k proof -> hash -> hash -> bool

  val pp
    :  (Format.formatter -> 'k -> unit)
    -> (Format.formatter -> 'v -> unit)
    -> ('k, 'v) t
    -> string

  val pp_proof : (Format.formatter -> 'k -> unit) -> 'k proof -> string

  val pp_op
    :  (Format.formatter -> 'k -> unit)
    -> (Format.formatter -> 'v -> unit)
    -> ('k, 'v) op
    -> string
end
