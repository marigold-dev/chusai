#include "../../stdlib_ext/src/stdlibext.mligo"

type hash_t = string

module H =  struct
   let combine (lhash : hash_t) (khash : hash_t) (vhash : hash_t) (rhash : hash_t) : hash_t = failwith "todo"
   let compute_hash (type k) (key : k) : hash_t = failwith "todo"
   let empty : hash_t = ""
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
let matches_hashes (type k) (proof : k proof) (initial_root_hash : hash_t) (final_root_hash : hash_t)
    : bool
  =
  let compute_hashes_for_remove ((initial_acc, final_acc) :hash_t * hash_t) (step : k remove_step) : (hash_t * hash_t)
    = 
    match step with
    | RemoveLeaf p -> H.combine H.empty (H.compute_hash p.key) p.vhash H.empty, H.empty
    | ReplaceWithLeftChild p ->
      let initial =
        H.combine p.initial_lhash (H.compute_hash p.initial_key) p.initial_vhash H.empty
      in
      let final =
        H.combine p.final_lhash (H.compute_hash p.final_key) p.final_vhash p.final_rhash
      in
      initial, final
    | ReplaceWithRightChild p ->
      let initial =
        H.combine H.empty (H.compute_hash p.initial_key) p.initial_vhash p.initial_rhash
      in
      let final =
        H.combine p.final_lhash (H.compute_hash p.final_key) p.final_vhash p.final_rhash
      in
      initial, final
    | ReplaceWithBiggestLeft p ->
      let initial =
        H.combine initial_acc (H.compute_hash p.initial_key) p.initial_vhash p.rhash
      in
      let final = H.combine final_acc (H.compute_hash p.final_key) p.final_vhash p.rhash in
      initial, final
  in
  let compute_hashes (step : k step) ((initial_acc, final_acc) :hash_t * hash_t) : hash_t * hash_t
    =
    match step with
    | NewLeaf p ->
      let initial = H.empty in
      let final = H.combine H.empty (H.compute_hash p.key) p.vhash H.empty in
      initial, final
    | GoLeft p ->
      let khash = H.compute_hash p.key in
      let initial = H.combine initial_acc khash p.vhash p.rhash in
      let final = H.combine final_acc khash p.vhash p.rhash in
      initial, final
    | GoRight p ->
      let khash = H.compute_hash p.key in
      let initial = H.combine p.lhash khash p.vhash initial_acc in
      let final = H.combine p.lhash khash p.vhash final_acc in
      initial, final
    | ValueChanged p ->
      let khash = H.compute_hash p.key in
      let initial = H.combine p.lhash khash p.initial_vhash p.rhash in
      let final = H.combine p.lhash khash p.final_vhash p.rhash in
      initial, final
    | Remove remove_proof ->
      compute_hashes_for_remove (initial_acc, final_acc) remove_proof
    | NotFound not_found -> not_found.thash, not_found.thash
    | Found p ->
      let khash = H.compute_hash p.key in
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
let rec consume_path (type k) (key : k) (step : k proof) : k proof option = 
  match step with
  | [] -> None
  | GoLeft current_step :: next_steps ->
    if current_step.key > key then consume_path key next_steps else None
  | GoRight current_step :: next_steps ->
    if current_step.key < key then consume_path key next_steps else None
  | remaining_path -> Some remaining_path


(** Given a key and a remove proof it checks if the replacement was done correctly.
    A correct replacement means that the replaced node's final key equals the replacement node key. 
*)
let is_valid_replacement (type k) (replaced_key : k) (proof : k proof) : bool =
  let rec go (acc : bool) (current_key : k) (current_proof : k proof) : bool =
    match current_proof with
    | [ Remove (RemoveLeaf remove_leaf) ] -> acc && remove_leaf.key = replaced_key
    | [ Remove (ReplaceWithLeftChild replace) ] ->
      acc && replace.initial_key = replaced_key
    | [ Remove (ReplaceWithRightChild replace) ] ->
      acc && replace.initial_key = replaced_key
    | Remove (ReplaceWithBiggestLeft replace) :: rest ->
      go (acc && replace.initial_key = replaced_key) replace.initial_key rest
    | GoRight go_right :: rest -> go acc replaced_key rest
    | _ -> false
  in
  go true replaced_key proof


(** True if the given proof is a valid proof for the given op *)
let matches_op (type k v) (proof : k proof) (op : (k, v) op) : bool =
    let result_opt =
      match op with
      | Upsert upsert_op ->
        Option.map 
          (fun (step : k step) ->
            match step with
            | [ NewLeaf new_leaf ] ->
              new_leaf.key = upsert_op.key
              && new_leaf.vhash = H.value upsert_op.value
            | [ ValueChanged updated_node ] ->
              updated_node.key = upsert_op.key
              && updated_node.final_vhash = H.value upsert_op.value
            | _ -> false)
          (consume_path upsert_op.key proof)
      | Remove remove_op ->
        Option.map 
          (fun (step : k step) ->
            match step with 
            | [ NotFound _ ] -> true
            | [ Remove (RemoveLeaf remove_leaf) ] -> remove_leaf.key = remove_op.key
            | [ Remove (ReplaceWithLeftChild replace) ] ->
              replace.initial_key = remove_op.key
              && replace.final_key < remove_op.key
            | [ Remove (ReplaceWithRightChild replace) ] ->
              replace.initial_key = remove_op.key
              && replace.final_key > remove_op.key
            | Remove (ReplaceWithBiggestLeft replace) :: proof_of_replacement ->
              replace.initial_key = remove_op.key
              && replace.final_key < remove_op.key
              && is_valid_replacement replace.final_key proof_of_replacement
            | _ -> false)
          (consume_path remove_op.key proof)
      | Lookup lookup_op ->
        Option.map 
          (fun (step : k step) ->
            match step with
            | [ Found found ] -> found.key = lookup_op.key
            | [ NotFound not_found ] -> true
            | _ -> false)
          (consume_path lookup_op.key proof)
    in
    OptionExt.default false result_opt
