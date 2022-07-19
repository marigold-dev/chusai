#import "../../stdlib_ext/src/stdlibext.mligo" "StdlibExt"
#include "../../commons/verifier_interface.mligo"

module H =  struct
  let combine (lhash : hash_t) (khash : hash_t) (vhash : hash_t) (rhash : hash_t) : hash_t = 
    Crypto.sha3 ( Bytes.concat lhash (Bytes.concat khash (Bytes.concat vhash rhash)))

  (** dummy implementation*)
  let empty : hash_t = Crypto.sha3 (Bytes.pack "sha256_empty") 
end

type ('k, 'v) op =
  | Lookup of { key : 'k }
  | Upsert of
      { key : 'k
      ; value : 'v
      }
  | Remove of { key : 'k }

type 'k remove_step =
  | ReplaceWithLeftChild of
      { initial_key : 'k
      ; initial_vhash : hash_t
      ; initial_lhash : hash_t
      ; final_key : 'k
      ; final_vhash : hash_t
      ; final_lhash : hash_t
      ; final_rhash : hash_t
      }
  | ReplaceWithRightChild of
      { initial_key : 'k
      ; initial_vhash : hash_t
      ; initial_rhash : hash_t
      ; final_key : 'k
      ; final_vhash : hash_t
      ; final_lhash : hash_t
      ; final_rhash : hash_t
      }
  | ReplaceWithBiggestLeft of
      { initial_key : 'k
      ; initial_vhash : hash_t
      ; final_key : 'k
      ; final_vhash : hash_t
      ; rhash : hash_t
      }
  | RemoveLeaf of
      { vhash : hash_t
      ; key : 'k
      }

type 'k step =
  | GoLeft of
      { key : 'k
      ; vhash : hash_t
      ; rhash : hash_t
      }
  | GoRight of
      { key : 'k
      ; vhash : hash_t
      ; lhash : hash_t
      }
  | ValueChanged of
      { initial_vhash : hash_t
      ; final_vhash : hash_t
      ; key : 'k
      ; lhash : hash_t
      ; rhash : hash_t
      }
  | NewLeaf of
      { vhash : hash_t
      ; key : 'k
      }
  | Remove of 'k remove_step
  | NotFound of { thash : hash_t }
  | Found of
      { key : 'k
      ; vhash : hash_t
      ; lhash : hash_t
      ; rhash : hash_t
      }

type 'k proof = 'k step list

(* True when the initial/final hashes computed from the proof match the given initial/final hashes *)
let matches_hashes (type k)  (hash_key : k -> hash_t) (proof : k proof) (initial_root_hash : hash_t) (final_root_hash : hash_t)
    : bool
  =
  let compute_hashes_for_remove ((initial_acc, final_acc) :hash_t * hash_t) (step : k remove_step) : (hash_t * hash_t)
    = 
    match step with
    | RemoveLeaf p -> H.combine H.empty (hash_key p.key) p.vhash H.empty, H.empty
    | ReplaceWithLeftChild p ->
      let initial =
        H.combine p.initial_lhash (hash_key p.initial_key) p.initial_vhash H.empty
      in
      let final =
        H.combine p.final_lhash (hash_key p.final_key) p.final_vhash p.final_rhash
      in
      initial, final
    | ReplaceWithRightChild p ->
      let initial =
        H.combine H.empty (hash_key p.initial_key) p.initial_vhash p.initial_rhash
      in
      let final =
        H.combine p.final_lhash (hash_key p.final_key) p.final_vhash p.final_rhash
      in
      initial, final
    | ReplaceWithBiggestLeft p ->
      let initial =
        H.combine initial_acc (hash_key p.initial_key) p.initial_vhash p.rhash
      in
      let final = H.combine final_acc (hash_key p.final_key) p.final_vhash p.rhash in
      initial, final
  in
  let compute_hashes ((step, (initial_acc, final_acc)) : (k step * (hash_t * hash_t))) : hash_t * hash_t
    =
    match step with
    | NewLeaf p ->
      let initial = H.empty in
      let final = H.combine H.empty (hash_key p.key) p.vhash H.empty in
      initial, final
    | GoLeft p ->
      let khash = hash_key p.key in
      let initial = H.combine initial_acc khash p.vhash p.rhash in
      let final = H.combine final_acc khash p.vhash p.rhash in
      initial, final
    | GoRight p ->
      let khash = hash_key p.key in
      let initial = H.combine p.lhash khash p.vhash initial_acc in
      let final = H.combine p.lhash khash p.vhash final_acc in
      initial, final
    | ValueChanged p ->
      let khash = hash_key p.key in
      let initial = H.combine p.lhash khash p.initial_vhash p.rhash in
      let final = H.combine p.lhash khash p.final_vhash p.rhash in
      initial, final
    | Remove remove_proof ->
      compute_hashes_for_remove (initial_acc, final_acc) remove_proof
    | NotFound not_found -> not_found.thash, not_found.thash
    | Found p ->
      let khash = hash_key p.key in
      let h = H.combine p.lhash khash p.vhash p.rhash in
      h, h
    (* no changes so intial and final hashes are the same *)
  in
  let (computed_initial_hash, computed_final_hash) = List.fold_right compute_hashes proof (H.empty, H.empty) in 
  computed_initial_hash = initial_root_hash && computed_final_hash = final_root_hash


(** Given a key and a proof it goes through the proof and consumes 
    the GoLeft/GoRight steps as long as they are valid i.e. for the left steps 
    the node's key is bigger and for the right steps the node's key is smaller
    If the path was valid and returns a Some value containing the remaining steps 
    from the original proof.
*)
let rec consume_path (type k) (compare_key : k -> k -> int) (key : k) (step : k proof) : k proof option = 
  match step with
  | [] -> None
  | GoLeft current_step :: next_steps ->
    if (compare_key current_step.key key) > 0
    then consume_path compare_key key next_steps 
    else None
  | GoRight current_step :: next_steps ->
    if compare_key current_step.key key < 0 
    then consume_path compare_key key next_steps 
    else None
  | remaining_path -> Some remaining_path


(** Given a key and a remove proof it checks if the replacement was done correctly.
    A correct replacement means that the replaced node's final key equals the replacement node key. 
*)
let is_valid_replacement (type k) (compare_key : k -> k -> int) (replaced_key : k) (proof : k proof) : bool =
  let rec go (acc : bool) (current_replaced_key : k) (current_proof : k proof) : bool =
    match current_proof with
    | [ Remove (RemoveLeaf remove_leaf) ] -> acc && compare_key remove_leaf.key current_replaced_key = 0
    | [ Remove (ReplaceWithLeftChild replace) ] ->
      acc && compare_key replace.initial_key current_replaced_key = 0
    | [ Remove (ReplaceWithRightChild replace) ] ->
      acc && compare_key replace.initial_key current_replaced_key = 0
    | Remove (ReplaceWithBiggestLeft replace) :: rest ->
      go (acc && compare_key replace.initial_key current_replaced_key = 0) replace.initial_key rest
    | GoRight _go_right :: rest -> go acc current_replaced_key rest
    | _ -> false
  in
  go true replaced_key proof


(** True if the given proof is a valid proof for the given op *)
let matches_op (type k v) (compare_key : k -> k -> int) (hash_value : v -> hash_t) (proof : k proof) (op : (k, v) op) : bool =
    let result_opt =
      match op with
      | Upsert upsert_op ->
        Option.map 
          (fun (remaining_proof : k proof) ->
            match remaining_proof with
            | [ NewLeaf new_leaf ] ->
              compare_key new_leaf.key upsert_op.key = 0
              && new_leaf.vhash = hash_value upsert_op.value
            | [ ValueChanged updated_node ] ->
              compare_key updated_node.key upsert_op.key = 0
              && updated_node.final_vhash = hash_value upsert_op.value
            | _ -> false)
          (consume_path compare_key upsert_op.key proof)
      | Remove remove_op ->
        Option.map 
          (fun (remaining_proof : k proof) ->
            match remaining_proof with
            | [ NotFound _ ] -> true
            | [ Remove (RemoveLeaf remove_leaf) ] -> compare_key remove_leaf.key remove_op.key = 0
            | [ Remove (ReplaceWithLeftChild replace) ] ->
              compare_key replace.initial_key remove_op.key = 0
              && compare_key replace.final_key remove_op.key < 0
            | [ Remove (ReplaceWithRightChild replace) ] ->
              compare_key replace.initial_key remove_op.key = 0
              && compare_key replace.final_key remove_op.key > 0
            | Remove (ReplaceWithBiggestLeft replace) :: proof_of_replacement ->
              compare_key replace.initial_key remove_op.key = 0
              && compare_key replace.final_key remove_op.key < 0
              && is_valid_replacement compare_key replace.final_key proof_of_replacement
            | _ -> false)
          (consume_path compare_key remove_op.key proof)
      | Lookup lookup_op ->
        Option.map 
          (fun (remaining_proof : k proof) ->
            match remaining_proof with
            | [ Found found ] -> compare_key found.key lookup_op.key = 0
            | [ NotFound _not_found ] -> true
            | _ -> false)
          (consume_path compare_key lookup_op.key proof)
    in
    StdlibExt.OptionExt.default result_opt false

(** Verifies that the given op is valid for the given proof and that the given hashes are valid for the given proof*)
let verify_proof
    (type k v)
    (verifier : (k, v) map_verifier)
    (op : (k, v) op)
    (proof : k proof)
    (initial_root_hash : hash_t)
    (final_root_hash : hash_t)
    : bool
  =
  (matches_op verifier.compare_key verifier.hash_value proof op) 
  && (matches_hashes verifier.hash_key proof initial_root_hash final_root_hash)
  