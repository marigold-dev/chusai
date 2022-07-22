#import "../../stdlib_ext/src/stdlibext.mligo" "StdlibExt"
#include "../../commons/verifier_interface.mligo"

module H =  struct
  let combine (lhash : hash_t) (khash : hash_t) (vhash : hash_t) (rhash : hash_t) : hash_t = 
    Crypto.sha3 ( Bytes.concat lhash (Bytes.concat khash (Bytes.concat vhash rhash)))

  (** dummy implementation*)
  let empty : hash_t = Crypto.sha3 (Bytes.pack "sha256_empty") 
end

(** Verifies that the given op is valid for the given proof and that the given hashes are valid for the given proof*)
let verify_proof
    (type k)
    (serialize_key : k -> bytes)
    (proof : k proof)
    (root_hash : hash_t)
    : bool
  =
  let hash_key (k : k) : hash_t = Crypto.sha3 (serialize_key k) in

  let compute_hash (step , acc_opt : k proof_step * hash_t option)
      : hash_t option
    =
    match acc_opt, step with
    | Some acc, GoLeft p ->
      let khash = hash_key p.key in
      Some (H.combine acc khash p.vhash p.rhash)
    | Some acc, GoRight p ->
      let khash = hash_key p.key in
      Some (H.combine p.lhash khash p.vhash acc)
    | None, NotFound -> 
      Some H.empty
    | None, Found p ->
      let khash = hash_key p.key in
      Some (H.combine p.lhash khash p.vhash p.rhash)
    | _ -> None
  in
  let computed_hash_opt = List.fold_right compute_hash proof (None : hash_t option) in
  Some root_hash = computed_hash_opt