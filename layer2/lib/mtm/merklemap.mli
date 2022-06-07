module type MerkleMap = 
sig
  type hash
  type ('k, 'v) t
  type proof

  val empty : (Format.formatter -> 'k -> unit) -> (Format.formatter -> 'v -> unit) -> ('k, 'v) t
  val from_list :  (Format.formatter -> 'k -> unit) -> (Format.formatter -> 'v -> unit) -> ('k * 'v) list -> ('k, 'v) t
  val to_list : ('k, 'v) t -> ('k * 'v) list
  
  val update_map : 'k -> ('v option -> 'v option) ->  ('k, 'v) t -> proof * ('k, 'v) t
  val upsert : 'k -> 'v ->  ('k, 'v) t -> proof * ('k, 'v) t
  val remove : 'k ->  ('k, 'v) t -> proof * ('k, 'v) t

  val root_hash : ('k, 'v) t -> hash
  val verify_proof : proof -> hash -> hash -> bool

  val show : ('k, 'v) t -> string
end