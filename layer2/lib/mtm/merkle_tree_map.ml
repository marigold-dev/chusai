open Intf

(* A map implemented as a Merkle-ized binary search tree 
The implementation is based on a non-balanced binary search tree.
During element removal the replacement is done with the biggest-left-child
   *)
module Make (Hash : HASH) : MERKLEMAP = struct
  type hash = Hash.t [@@deriving show]
  type khash = KHash of hash [@@deriving show]
  type vhash = VHash of hash [@@deriving show]
  type thash = THash of hash [@@deriving show]

  type ('k, 'v) tnode =
    { key : 'k
    ; value : 'v
    ; khash : khash
    ; vhash : vhash
    ; left : ('k, 'v) hnode option
    ; right : ('k, 'v) hnode option
    }
  [@@deriving show]

  and ('k, 'v) hnode =
    { thash : thash
    ; content : ('k, 'v) tnode
    }
  [@@deriving show]

  type ('k, 'v) op =
    | Lookup of { key : 'k }
    | Upsert of
        { key : 'k
        ; value : 'v
        }
    | Remove of { key : 'k }
  [@@deriving show]

  module H = struct
    let key k = KHash (Hash.hash k)
    let value v = VHash (Hash.hash v)

    let combine
        (THash lhash : thash)
        (KHash khash : khash)
        (VHash vhash : vhash)
        (THash rhash : thash)
      =
      let h = Hash.(lhash ++ khash ++ vhash ++ rhash) in
      THash h
    ;;

    let tempty : thash = THash Hash.empty

    let node_hash (node_opt : ('k, 'v) hnode option) : thash =
      node_opt |> Option.fold ~none:tempty ~some:(fun hnode -> hnode.thash)
    ;;

    let node (tnode : ('k, 'v) tnode) =
      { thash =
          combine (node_hash tnode.left) tnode.khash tnode.vhash (node_hash tnode.right)
      ; content = tnode
      }
    ;;
  end

  module Proof = struct
    type 'k remove_step =
      | ReplaceWithLeftChild of
          { initial_key : 'k
          ; initial_vhash : vhash
          ; initial_lhash : thash
          ; final_key : 'k
          ; final_vhash : vhash
          ; final_lhash : thash
          ; final_rhash : thash
          }
      | ReplaceWithRightChild of
          { initial_key : 'k
          ; initial_vhash : vhash
          ; initial_rhash : thash
          ; final_key : 'k
          ; final_vhash : vhash
          ; final_lhash : thash
          ; final_rhash : thash
          }
      | ReplaceWithBiggestLeft of
          { initial_key : 'k
          ; initial_vhash : vhash
          ; final_key : 'k
          ; final_vhash : vhash
          ; rhash : thash
          }
      | RemoveLeaf of
          { vhash : vhash
          ; key : 'k
          }
    [@@deriving show]

    type 'k step =
      | GoLeft of
          { key : 'k
          ; vhash : vhash
          ; rhash : thash
          }
      | GoRight of
          { key : 'k
          ; vhash : vhash
          ; lhash : thash
          }
      | ValueChanged of
          { initial_vhash : vhash
          ; final_vhash : vhash
          ; key : 'k
          ; lhash : thash
          ; rhash : thash
          }
      | NewLeaf of
          { vhash : vhash
          ; key : 'k
          }
      | Remove of 'k remove_step
      | NotFound of { thash : thash }
      | Found of
          { key : 'k
          ; vhash : vhash
          ; lhash : thash
          ; rhash : thash
          }
    [@@deriving show]

    type 'k t = 'k step list

    (*True when the initial/final hashes computed from the proof match the given initial/final hashes *)
    let matches_hashes (proof : 'k t) (initial_root_hash : hash) (final_root_hash : hash)
        : bool
      =
      let compute_hashes_for_remove ((initial_acc, final_acc) : thash * thash)
          : 'k remove_step -> thash * thash
        = function
        | RemoveLeaf p -> H.combine H.tempty (H.key p.key) p.vhash H.tempty, H.tempty
        | ReplaceWithLeftChild p ->
          let initial =
            H.combine p.initial_lhash (H.key p.initial_key) p.initial_vhash H.tempty
          in
          let final =
            H.combine p.final_lhash (H.key p.final_key) p.final_vhash p.final_rhash
          in
          initial, final
        | ReplaceWithRightChild p ->
          let initial =
            H.combine H.tempty (H.key p.initial_key) p.initial_vhash p.initial_rhash
          in
          let final =
            H.combine p.final_lhash (H.key p.final_key) p.final_vhash p.final_rhash
          in
          initial, final
        | ReplaceWithBiggestLeft p ->
          let initial =
            H.combine initial_acc (H.key p.initial_key) p.initial_vhash p.rhash
          in
          let final = H.combine final_acc (H.key p.final_key) p.final_vhash p.rhash in
          initial, final
      in
      let compute_hashes (step : 'k step) ((initial_acc, final_acc) : thash * thash)
          : thash * thash
        =
        match step with
        | NewLeaf p ->
          let initial = H.tempty in
          let final = H.combine H.tempty (H.key p.key) p.vhash H.tempty in
          initial, final
        | GoLeft p ->
          let khash = H.key p.key in
          let initial = H.combine initial_acc khash p.vhash p.rhash in
          let final = H.combine final_acc khash p.vhash p.rhash in
          initial, final
        | GoRight p ->
          let khash = H.key p.key in
          let initial = H.combine p.lhash khash p.vhash initial_acc in
          let final = H.combine p.lhash khash p.vhash final_acc in
          initial, final
        | ValueChanged p ->
          let khash = H.key p.key in
          let initial = H.combine p.lhash khash p.initial_vhash p.rhash in
          let final = H.combine p.lhash khash p.final_vhash p.rhash in
          initial, final
        | Remove remove_proof ->
          compute_hashes_for_remove (initial_acc, final_acc) remove_proof
        | NotFound not_found -> not_found.thash, not_found.thash
        | Found p ->
          let khash = H.key p.key in
          let h = H.combine p.lhash khash p.vhash p.rhash in
          h, h
        (* no changes so intial and final hashes are the same *)
      in
      let THash computed_initial_hash, THash computed_final_hash =
        List.fold_right compute_hashes proof (H.tempty, H.tempty)
      in
      computed_initial_hash = initial_root_hash && computed_final_hash = final_root_hash
    ;;

    (* Given a key and a proof it goes through the proof and consumes 
       the GoLeft/GoRight steps as long as they are valid i.e. for the left steps 
       the node's key is bigger and for the right steps the node's key is smaller
       If the path was valid and returns a Some value containing the remaining steps 
       from the original proof.
    *)
    let rec consume_path (key : 'k) : 'k t -> 'k t option = function
      | [] -> None
      | GoLeft current_step :: next_steps ->
        if current_step.key > key then consume_path key next_steps else None
      | GoRight current_step :: next_steps ->
        if current_step.key < key then consume_path key next_steps else None
      | remaining_path -> Some remaining_path 
    ;;

    (* given a key and a remove proof it checks if the replacement was done correctly.
       A correct replacement means that the replaced node's final key equals the replacement node key. 
    *)
    let is_valid_replacement (replaced_key : 'k) (proof : 'k t) : bool =
      let rec go (acc : bool) (current_replaced_key : 'k) (current_proof : 'k t) : bool =
        match current_proof with
        | [ Remove (RemoveLeaf remove_leaf) ] -> acc && remove_leaf.key == current_replaced_key
        | [ Remove (ReplaceWithLeftChild replace) ] ->
          acc && replace.initial_key == current_replaced_key
        | [ Remove (ReplaceWithRightChild replace) ] ->
          acc && replace.initial_key == current_replaced_key
        | Remove (ReplaceWithBiggestLeft replace) :: rest ->
          go (acc && replace.initial_key == current_replaced_key) replace.initial_key rest
        | GoRight go_right :: rest -> 
          go (acc && go_right.key < current_replaced_key) current_replaced_key rest
        | _ -> false
      in
      go true replaced_key proof
    ;;

    (** True if the given proof is a valid proof for the given op 
       *)
    let matches_op (proof : 'k t) (op : ('k, 'v) op) : bool =
      let result =
        match op with
        | Upsert upsert_op ->
          consume_path upsert_op.key proof
          |> Option.map (function
                 | [ NewLeaf new_leaf ] ->
                   new_leaf.key = upsert_op.key
                   && new_leaf.vhash = H.value upsert_op.value
                 | [ ValueChanged updated_node ] ->
                   updated_node.key = upsert_op.key
                   && updated_node.final_vhash = H.value upsert_op.value
                 | _ -> false)
        | Remove remove_op ->
          consume_path remove_op.key proof
          |> Option.map (function
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
        | Lookup lookup_op ->
          consume_path lookup_op.key proof
          |> Option.map (function 
                 | [ Found found ] -> found.key = lookup_op.key
                 | [ NotFound _ ] -> true
                 | _ -> false)
      in
      CCOption.get_or ~default:false result
    ;;
  end

  type ('k, 'v) t = MerkleTreeMap of ('k, 'v) hnode option
  type 'k proof = 'k Proof.t

  let empty : ('k, 'v) t = MerkleTreeMap None

  let root_hash (MerkleTreeMap map : ('k, 'v) t) : hash =
    let (THash h) = H.node_hash map in
    h
  ;;

  let insert (key : 'k) (value : 'v) : 'k proof * ('k, 'v) hnode option =
    let khash = H.key key in
    let vhash = H.value value in
    let proof = [ Proof.NewLeaf { key; vhash } ] in
    let n = H.node { key; value; khash; vhash; left = None; right = None } in
    proof, Some n
  ;;

  (* Returns the content (i.e. k,v) of the biggest child in the subtree starting at current_node, the proof and the subtree with the biggest child removed *)
  let rec extract_biggest_child (current_node : ('k, 'v) hnode)
      : 'k proof * 'k * 'v * ('k, 'v) hnode option
    =
    match current_node.content.left, current_node.content.right with
    | None, None ->
      let proof_step =
        Proof.Remove
          (Proof.RemoveLeaf
             { key = current_node.content.key; vhash = current_node.content.vhash })
      in
      [ proof_step ], current_node.content.key, current_node.content.value, None
    | Some left, None ->
      let proof_step =
        Proof.Remove
          (Proof.ReplaceWithLeftChild
             { initial_key = current_node.content.key
             ; initial_vhash = current_node.content.vhash
             ; initial_lhash = H.node_hash current_node.content.left
             ; final_key = left.content.key
             ; final_vhash = left.content.vhash
             ; final_lhash = H.node_hash left.content.left
             ; final_rhash = H.node_hash left.content.right
             })
      in
      ( [ proof_step ]
      , current_node.content.key
      , current_node.content.value
      , current_node.content.left )
    | _, Some right ->
      let child_proof, biggest_key, biggest_value, right_wihout_biggest_child =
        extract_biggest_child right
      in
      let proof_step =
        Proof.GoRight
          { key = current_node.content.key
          ; vhash = current_node.content.vhash
          ; lhash = H.node_hash current_node.content.left
          }
      in
      let proof = proof_step :: child_proof in
      let current_without_biggest_child =
        H.node { current_node.content with right = right_wihout_biggest_child }
      in
      proof, biggest_key, biggest_value, Some current_without_biggest_child
  ;;

  (* Returns op, proof and a replacement for the current node, considering that the current_node is removed from the tree. 
        In case of a leaf the returned node is None so there's no replacement. 
        In case of a non-leaf the returned node is left child if there is no right child or right child if there is no left child.
        If both children exist then the the rotation with biggest-left-child happens 
        *)
  let remove_node (current_node : ('k, 'v) hnode) : 'k proof * ('k, 'v) hnode option =
    let proof, node =
      match current_node.content.left, current_node.content.right with
      | None, None ->
        let proof_step =
          Proof.Remove
            (Proof.RemoveLeaf
               { vhash = current_node.content.vhash; key = current_node.content.key })
        in
        [ proof_step ], None
      | None, Some right_child ->
        let proof_step =
          Proof.Remove
            (Proof.ReplaceWithRightChild
               { initial_key = current_node.content.key
               ; initial_vhash = current_node.content.vhash
               ; initial_rhash = right_child.thash
               ; final_key = right_child.content.key
               ; final_vhash = right_child.content.vhash
               ; final_lhash = H.node_hash right_child.content.left
               ; final_rhash = H.node_hash right_child.content.right
               })
        in
        let n = H.node right_child.content in
        [ proof_step ], Some n
      | Some left_child, None ->
        let proof_step =
          Proof.Remove
            (Proof.ReplaceWithLeftChild
               { initial_key = current_node.content.key
               ; initial_vhash = current_node.content.vhash
               ; initial_lhash = left_child.thash
               ; final_key = left_child.content.key
               ; final_vhash = left_child.content.vhash
               ; final_lhash = H.node_hash left_child.content.left
               ; final_rhash = H.node_hash left_child.content.right
               })
        in
        [ proof_step ], Some left_child
      | Some original_left, Some right ->
        let replacement_proof, replacement_key, replacement_value, new_left =
          extract_biggest_child original_left
        in
        let replacement_vhash = H.value replacement_value in
        let proof_step =
          Proof.Remove
            (Proof.ReplaceWithBiggestLeft
               { initial_key = current_node.content.key
               ; initial_vhash = current_node.content.vhash
               ; final_key = replacement_key
               ; final_vhash = replacement_vhash
               ; rhash = right.thash
               })
        in
        let proof = proof_step :: replacement_proof in
        let n =
          H.node
            { key = replacement_key
            ; value = replacement_value
            ; khash = H.key replacement_key
            ; vhash = replacement_vhash
            ; left = new_left
            ; right = current_node.content.right
            }
        in
        proof, Some n
    in
    proof, node
  ;;

  (* Replaces the value in the current_node with the new_value. 
       A new node is always returned.*)
  let update (current_node : ('k, 'v) hnode) (new_value : 'v)
      : 'k proof * ('k, 'v) hnode option
    =
    let new_vhash = H.value new_value in
    let proof_step =
      Proof.ValueChanged
        { initial_vhash = current_node.content.vhash
        ; final_vhash = new_vhash
        ; key = current_node.content.key
        ; lhash = H.node_hash current_node.content.left
        ; rhash = H.node_hash current_node.content.right
        }
    in
    let n = H.node { current_node.content with value = new_value; vhash = new_vhash } in
    [ proof_step ], Some n
  ;;

  (* This implements a generic operation on the tree with the given root. It returns the following:
     1. the operation executed
     2. the proof for the above operation
     3. the updated version of the root, if the operation did any updates, or the original root, if nothing has changed
     4. the result of the operation (ex: the found value in case of a lookup)*)
  let apply_on_node
      (type k v)
      (handle_key_not_found : thash -> k proof * (k, v) hnode option)
      (handle_key_found : (k, v) hnode -> k proof * (k, v) hnode option)
      (key : k)
      (root : (k, v) hnode option)
      : v option * k proof * (k, v) hnode option
    =
    (* Updates a possibly missing node. 
          If the node is empty then we try to insert. If not empty then we try to update its content *)
    let rec update_opt_node (current_node_opt : (k, v) hnode option)
        : v option * k proof * (k, v) hnode option
      =
      match current_node_opt with 
      | None ->
        let proof, new_node = handle_key_not_found @@ H.node_hash current_node_opt in
        None, proof, new_node
       | Some node ->
        let value, proof, new_node = update_non_empty_node node in
        value, proof, new_node
    (* We reached a situation when the node with the given key was not found. 
          If the updater return a value we need to create a new leaf for it. Otherwise nothing will happen *)
    (* Some value in the subtree starting at current_node needs to be updated. 
          Depending on the key value we will update the current node or the left subtree or the right subtree *)
    and update_non_empty_node (current_node : (k, v) hnode)
        : v option * k proof * (k, v) hnode option
      =
      if current_node.content.key = key
      then (
        let proof, new_node = handle_key_found current_node in
        Some current_node.content.value, proof, new_node)
      else if current_node.content.key < key
      then update_right_subtree current_node
      else (*i.e. current_node.key > key *) update_left_subtree current_node
    (* We found the node that needs to be updated. 
          If the updater returns a Some new value then we replace the old value in the node with the new one.
          If the updater returns None then we remove the current_node*)
    (* Go right with the search.*)
    and update_right_subtree (current_node : (k, v) hnode)
        : v option * k proof * (k, v) hnode option
      =
      let result, update_right_proof, new_right_node =
        update_opt_node current_node.content.right
      in
      let proof_step =
        Proof.GoRight
          { lhash = H.node_hash current_node.content.left
          ; key = current_node.content.key
          ; vhash = current_node.content.vhash
          }
      in
      let proof = proof_step :: update_right_proof in
      let n = H.node { current_node.content with right = new_right_node } in
      result, proof, Some n
    (* Go left with the search *)
    and update_left_subtree (current_node : (k, v) hnode)
        : v option * k proof * (k, v) hnode option
      =
      let result, update_left_proof, new_left_node =
        update_opt_node current_node.content.left
      in
      let proof_step =
        Proof.GoLeft
          { rhash = H.node_hash current_node.content.right
          ; key = current_node.content.key
          ; vhash = current_node.content.vhash
          }
      in
      let proof = proof_step :: update_left_proof in
      let n = H.node { current_node.content with left = new_left_node } in
      result, proof, Some n
    in
    update_opt_node root
  ;;

  let lookup (key : 'k) (root : ('k, 'v) hnode option)
      : 'v option * 'k proof * ('k, 'v) hnode option
    =
    let handle_key_not_found (current_node_thash : thash)
        : 'k proof * ('k, 'v) hnode option
      =
      [ Proof.NotFound { thash = current_node_thash } ], root
    in
    let handle_key_found (node : ('k, 'v) hnode) : 'k proof * ('k, 'v) hnode option =
      let proof_step =
        Proof.Found
          { key = node.content.key
          ; lhash = H.node_hash node.content.left
          ; rhash = H.node_hash node.content.right
          ; vhash = node.content.vhash
          }
      in
      [ proof_step ], root
    in
    apply_on_node handle_key_not_found handle_key_found key root
  ;;

  let upsert (key : 'k) (value : 'v) (root : ('k, 'v) hnode option)
      : 'v option * 'k proof * ('k, 'v) hnode option
    =
    let handle_key_not_found _thash : 'k proof * ('k, 'v) hnode option =
      insert key value
    in
    let handle_key_found (current_node : ('k, 'v) hnode)
        : 'k proof * ('k, 'v) hnode option
      =
      update current_node value
    in
    apply_on_node handle_key_not_found handle_key_found key root
  ;;

  let remove (key : 'k) (root : ('k, 'v) hnode option)
      : 'v option * 'k proof * ('k, 'v) hnode option
    =
    let handle_key_not_found thash : 'k proof * ('k, 'v) hnode option =
      [ Proof.NotFound { thash } ], None
    in
    let handle_key_found (current_node : ('k, 'v) hnode)
        : 'k proof * ('k, 'v) hnode option
      =
      remove_node current_node
    in
    apply_on_node handle_key_not_found handle_key_found key root
  ;;

  let execute (op : ('k, 'v) op) (MerkleTreeMap mtm : ('k, 'v) t)
      : 'v option * 'k proof * ('k, 'v) t
    =
    let result, proof, new_root =
      match op with
      | Lookup lookup_op -> lookup lookup_op.key mtm
      | Upsert upsert_op -> upsert upsert_op.key upsert_op.value mtm
      | Remove remove_op -> remove remove_op.key mtm
    in
    result, proof, MerkleTreeMap new_root
  ;;

  (* Verifies that the given op is valid for the given proof and that the given hashes are valid for the given proof*)
  let verify_proof
      (op : ('k, 'v) op)
      (proof : 'k proof)
      (initial_root_hash : hash)
      (final_root_hash : hash)
      : bool
    =
    let matches_op = Proof.matches_op proof op in
    let matches_hashes = Proof.matches_hashes proof initial_root_hash final_root_hash in
    matches_op && matches_hashes
  ;;

  let of_seq (kvs : ('k * 'v) Seq.t) : ('k, 'v) t =
    Seq.fold_left
      (fun acc (key, value) ->
        let _, _proof, map = execute (Upsert { key; value }) acc in
        map)
      empty
      kvs
  ;;

  let to_seq (MerkleTreeMap t : ('k, 'v) t) : ('k * 'v) Seq.t =
    let rec go (node_opt : ('k, 'v) hnode option) : ('k * 'v) Seq.t =
      node_opt
      |> Option.map (fun node ->
              let left = go node.content.left in
              let current = Seq.return (node.content.key, node.content.value) in
              let right = go node.content.right in
              Seq.append left @@ Seq.append current right)
      |> Option.to_seq
      |> CCSeq.flatten
    in
    go t
  ;;

  let pp
      (kfmt : Format.formatter -> 'k -> unit)
      (vfmt : Format.formatter -> 'v -> unit)
      (MerkleTreeMap t : ('k, 'v) t)
      : string
    =
    "MerkleTreeMap ("
    ^ Option.fold ~none:"Empty" ~some:(fun root -> show_hnode kfmt vfmt root) t
    ^ ")"
  ;;

  let pp_proof (kfmt : Format.formatter -> 'k -> unit) (proof : 'k proof) : string =
    let steps_strs = List.map (Proof.show_step kfmt) proof in
    "Proof [" ^ String.concat ";\n" steps_strs ^ "]"
  ;;

  let pp_op
      (kfmt : Format.formatter -> 'k -> unit)
      (vfmt : Format.formatter -> 'v -> unit)
      (op : ('k, 'v) op)
      : string
    =
    "Op (" ^ show_op kfmt vfmt op ^ ")"
  ;;
end
