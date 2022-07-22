module type HASH = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val empty : t
  val hash : 'a -> t
  val ( ++ ) : t -> t -> t
end

module type SERIALIZER = sig
  (** Serialize any value as bytes *)
  val serialize : 'a -> bytes
end

(** A STORAGE represented as a "merkle map"*)
module type MERKLEMAP = sig
  type ('k, 'v) t
  type 'k proof

  (** the hash type that is used by this storage *)
  type hash

  (** Creates an empty map *)
  val empty : ('k, 'v) t

  (** Creates a map from a sequence of key-value pairs *)
  val of_seq : ('k * 'v) Seq.t -> ('k, 'v) t

  (** Produces a sequence containg key-value pairs in ascending key order *)
  val to_seq : ('k, 'v) t -> ('k * 'v) Seq.t

  (** Returns the root hash of the tree *)
  val root_hash : ('k, 'v) t -> hash

  val upsert : 'k -> 'v -> ('k, 'v) t -> 'v option * ('k, 'v) t

  val lookup : 'k -> ('k, 'v) t -> 'v option * 'k proof

  (** Verifies an existence proof given the root hash and the proof *)
  val verify_proof : 'k proof -> hash -> bool

  (** Pretty printer for the map *)
  val pp
    :  (Format.formatter -> 'k -> unit)
    -> (Format.formatter -> 'v -> unit)
    -> ('k, 'v) t
    -> string

  (** Pretty printer for the proof type *)
  val pp_proof 
    : (Format.formatter -> 'k -> unit) 
    -> 'k proof 
    -> string
end
