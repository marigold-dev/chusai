module type MerkleMap = 
sig
  type hash
  type ('k, 'v) t
  type 'k proof

  type ('k, 'v) op 
  
  val empty : (Format.formatter -> 'k -> unit) -> (Format.formatter -> 'v -> unit) -> ('k, 'v) t
  val from_list :  (Format.formatter -> 'k -> unit) -> (Format.formatter -> 'v -> unit) -> ('k * 'v) list -> ('k, 'v) t
  val to_list : ('k, 'v) t -> ('k * 'v) list
  
  val update_map : 'k -> ('v option -> 'v option) ->  ('k, 'v) t -> ('k, 'v) op * 'k proof * ('k, 'v) t
  val upsert : 'k -> 'v ->  ('k, 'v) t -> ('k, 'v) op * 'k proof * ('k, 'v) t
  val remove : 'k ->  ('k, 'v) t -> ('k, 'v) op * 'k proof * ('k, 'v) t

  val root_hash : ('k, 'v) t -> hash
  val verify_proof : (Format.formatter -> 'k -> unit) -> (Format.formatter -> 'v -> unit) -> ('k, 'v) op -> 'k proof -> hash -> hash -> bool

  val show : ('k, 'v) t -> string
end