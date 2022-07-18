module type HASH = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val empty : t
  val hash : 'a -> t
  val ( ++ ) : t -> t -> t
end

module type SERIALIZER = sig
  val serialize : 'a -> bytes
end

(* Generic merkleized key-value storage type*)
module type STORAGE = sig
  (* The storage *)
  type ('k, 'v) t

  (* the operations available on this storage*)
  type ('k, 'v) op

  (* the proof type *)
  type 'k proof

  (* Execute a storage operation and returns:
     - maybe a value, depending on the operation:
      Lookup -> return a value if the key was found or None 
      Upsert -> return the original value if the key was found or None if it wasn't
      Remove -> return the original value if the key was found or None if it wasn't
     - the Merkle proof of the execution
     - the new version of the storage
     *)
  val execute : ('k, 'v) op -> ('k, 'v) t -> 'v option * 'k proof * ('k, 'v) t
end

(* A STORAGE represented as a "merkle map"*)
module type MERKLEMAP = sig
  type ('k, 'v) op =
    | Lookup of { key : 'k }
    | Upsert of
        { key : 'k
        ; value : 'v
        }
    | Remove of { key : 'k }

  type hash

  include STORAGE with type ('k, 'v) op := ('k, 'v) op

  val empty : ('k, 'v) t
  val of_seq : ('k * 'v) Seq.t -> ('k, 'v) t
  val to_seq : ('k, 'v) t -> ('k * 'v) Seq.t
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
