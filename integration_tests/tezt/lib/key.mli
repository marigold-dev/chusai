open Tezos_crypto

type t =
  { alias : string
  ; public_key_hash : string
  ; public_key : string
  ; secret_key : string
  }

val use : watermark:Signature.watermark -> signer:t -> bytes -> Signature.t
val write : directory:string -> t list -> unit
val activator : t
val sample : t array
val all : t list
