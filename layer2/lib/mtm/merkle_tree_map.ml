open Merklemap
open Optionext

(* A map implemented as a Merkle-ized binary search tree *)
module Make (Hash : Hash_algo.HASH) : MERKLEMAP = struct
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
    | Insert of
        { key : 'k
        ; value : 'v
        }
    | Update of
        { key : 'k
        ; old_value : 'v
        ; new_value : 'v
        }
    | Remove of { key : 'k }
    | Noop of { key : 'k }
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
    type 'k remove_proof =
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
          ; replacement_proof_left : 'k t
          }
      | RemoveLeaf of
          { vhash : vhash
          ; key : 'k
          }
    [@@deriving show]

    and 'k t =
      | GoLeft of
          { left_proof : 'k t
          ; key : 'k
          ; vhash : vhash
          ; rhash : thash
          }
      | GoRight of
          { right_proof : 'k t
          ; key : 'k
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
      | Remove of 'k remove_proof
      | NoChange of thash
      | NotFound
      | Found of
          { key : 'k
          ; vhash : vhash
          ; lhash : thash
          ; rhash : thash
          }
    [@@deriving show]

    (*True when the initial/final hashes computed from the proof match the given initial/final hashes *)
    let matches_hashes (proof : 'k t) (initial_root_hash : hash) (final_root_hash : hash)
        : bool
      =
      let rec compute_hashes (current_proof : 'k t) : thash * thash =
        let initial, final =
          match current_proof with
          | NewLeaf p -> H.tempty, H.combine H.tempty (H.key p.key) p.vhash H.tempty
          | GoLeft p ->
            let initial_lhash, final_lhash = compute_hashes p.left_proof in
            let khash = H.key p.key in
            let initial = H.combine initial_lhash khash p.vhash p.rhash in
            let final = H.combine final_lhash khash p.vhash p.rhash in
            initial, final
          | GoRight p ->
            let initial_rhash, final_rhash = compute_hashes p.right_proof in
            let khash = H.key p.key in
            let initial = H.combine p.lhash khash p.vhash initial_rhash in
            let final = H.combine p.lhash khash p.vhash final_rhash in
            initial, final
          | ValueChanged p ->
            let khash = H.key p.key in
            let initial = H.combine p.lhash khash p.initial_vhash p.rhash in
            let final = H.combine p.lhash khash p.final_vhash p.rhash in
            initial, final
          | Remove remove_proof -> compute_hashes_remove remove_proof
          | NoChange h -> h, h
          | NotFound -> H.tempty, H.tempty
          | Found p ->
            let khash = H.key p.key in
            let h = H.combine p.lhash khash p.vhash p.rhash in
            h, h
          (* no changes so intial and final hashes are the same *)
        in
        initial, final
      and compute_hashes_remove = function
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
          let replacement_initial, replacement_final =
            compute_hashes p.replacement_proof_left
          in
          let initial =
            H.combine replacement_initial (H.key p.initial_key) p.initial_vhash p.rhash
          in
          let final =
            H.combine replacement_final (H.key p.final_key) p.final_vhash p.rhash
          in
          initial, final
      in
      let THash computed_initial_hash, THash computed_final_hash = compute_hashes proof in
      computed_initial_hash = initial_root_hash && computed_final_hash = final_root_hash
    ;;

    (* True if the given proof is a valid proof for the given op*)
    let matches_op (proof : 'k t) (op : ('k, 'v) op) : bool =
      match op with
      | Noop noop ->
        let rec is_valid_noop = function
          | NoChange _ -> true
          | GoLeft go_left -> go_left.key > noop.key && is_valid_noop go_left.left_proof
          | GoRight go_right ->
            go_right.key < noop.key && is_valid_noop go_right.right_proof
          | _ -> false
        in
        is_valid_noop proof
      | Insert insert_op ->
        let rec is_valid_insert = function
          | GoLeft go_left ->
            go_left.key > insert_op.key && is_valid_insert go_left.left_proof
          | GoRight go_right ->
            go_right.key < insert_op.key && is_valid_insert go_right.right_proof
          | NewLeaf new_leaf ->
            new_leaf.key = insert_op.key && new_leaf.vhash = H.value insert_op.value
          | _ -> false
        in
        is_valid_insert proof
      | Update update_op ->
        let rec is_valid_update = function
          | GoLeft go_left ->
            go_left.key > update_op.key && is_valid_update go_left.left_proof
          | GoRight go_right ->
            go_right.key < update_op.key && is_valid_update go_right.right_proof
          | ValueChanged updated_node ->
            updated_node.key = update_op.key
            && updated_node.final_vhash = H.value update_op.new_value
            && updated_node.initial_vhash = H.value update_op.old_value
          | _ -> false
        in
        is_valid_update proof
      | Remove remove_op ->
        let rec is_valid_remove = function
          | GoLeft go_left ->
            go_left.key > remove_op.key && is_valid_remove go_left.left_proof
          | GoRight go_right ->
            go_right.key < remove_op.key && is_valid_remove go_right.right_proof
          | Remove remove_proof ->
            (match remove_proof with
            | RemoveLeaf remove_leaf -> remove_leaf.key = remove_op.key
            | ReplaceWithBiggestLeft replace ->
              replace.initial_key = remove_op.key && replace.final_key < remove_op.key
            | ReplaceWithLeftChild replace ->
              replace.initial_key = remove_op.key && replace.final_key < remove_op.key
            | ReplaceWithRightChild replace ->
              replace.initial_key = remove_op.key && replace.final_key > remove_op.key)
          | _ -> false
        in
        is_valid_remove proof
      | Lookup lookup_op ->
        let rec is_valid_lookup = function
          | GoLeft go_left ->
            go_left.key > lookup_op.key && is_valid_lookup go_left.left_proof
          | GoRight go_right ->
            go_right.key < lookup_op.key && is_valid_lookup go_right.right_proof
          | Found found -> found.key = lookup_op.key
          | NotFound -> true
          | _ -> false
        in
        is_valid_lookup proof
    ;;
  end

  type ('k, 'v) t = MerkleTreeMap of ('k, 'v) hnode option
  type 'k proof = 'k Proof.t

  let empty : ('k, 'v) t = MerkleTreeMap None

  let root_hash (MerkleTreeMap map : ('k, 'v) t) : hash =
    let (THash h) = H.node_hash map in
    h
  ;;

  let insert (key : 'k) (value : 'v) : ('k, 'v) op * 'k proof * ('k, 'v) hnode option =
    let khash = H.key key in
    let vhash = H.value value in
    let p = Proof.NewLeaf { key; vhash } in
    let n = H.node { key; value; khash; vhash; left = None; right = None } in
    Insert { key; value }, p, Some n
  ;;

  (* Returns the content (i.e. k,v) of the biggest child in the subtree starting at current_node, the proof and the subtree with the biggest child removed *)
  let rec extract_biggest_child (current_node : ('k, 'v) hnode)
      : 'k proof * 'k * 'v * ('k, 'v) hnode option
    =
    match current_node.content.left, current_node.content.right with
    | None, None ->
      let proof =
        Proof.Remove
          (Proof.RemoveLeaf
             { key = current_node.content.key; vhash = current_node.content.vhash })
      in
      proof, current_node.content.key, current_node.content.value, None
    | Some left, None ->
      let proof =
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
      ( proof
      , current_node.content.key
      , current_node.content.value
      , current_node.content.left )
    | _, Some right ->
      let child_proof, biggest_key, biggest_value, right_wihout_biggest_child =
        extract_biggest_child right
      in
      let proof =
        Proof.GoRight
          { right_proof = child_proof
          ; key = current_node.content.key
          ; vhash = current_node.content.vhash
          ; lhash = H.node_hash current_node.content.left
          }
      in
      let current_without_biggest_child =
        H.node { current_node.content with right = right_wihout_biggest_child }
      in
      proof, biggest_key, biggest_value, Some current_without_biggest_child
  ;;

  (* Returns op, proof and a replacement for the current node, considering that the current_node is removed from the tree. 
        In case of a leaf the returned node is None so there's no replacement. 
        In case of a non-leaf the returned node is either the left/right child if the replacement is possible  *)
  let remove_node (current_node : ('k, 'v) hnode)
      : ('k, 'v) op * 'k proof * ('k, 'v) hnode option
    =
    let proof, node =
      match current_node.content.left, current_node.content.right with
      | None, None ->
        let p =
          Proof.Remove
            (Proof.RemoveLeaf
               { vhash = current_node.content.vhash; key = current_node.content.key })
        in
        p, None
      | None, Some right_child ->
        let p =
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
        p, Some n
      | Some left_child, None ->
        let p =
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
        p, Some left_child
      | Some original_left, Some right ->
        let replacement_proof, replacement_key, replacement_value, new_left =
          extract_biggest_child original_left
        in
        let replacement_vhash = H.value replacement_value in
        let p =
          Proof.Remove
            (Proof.ReplaceWithBiggestLeft
               { initial_key = current_node.content.key
               ; initial_vhash = current_node.content.vhash
               ; final_key = replacement_key
               ; final_vhash = replacement_vhash
               ; rhash = right.thash
               ; replacement_proof_left = replacement_proof
               })
        in
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
        p, Some n
    in
    Remove { key = current_node.content.key }, proof, node
  ;;

  (* Replaces the value in the current_node with the new_value. 
       A new node is always returned.*)
  let replace_value (current_node : ('k, 'v) hnode) (new_value : 'v)
      : ('k, 'v) op * 'k proof * ('k, 'v) hnode option
    =
    let new_vhash = H.value new_value in
    let p =
      Proof.ValueChanged
        { initial_vhash = current_node.content.vhash
        ; final_vhash = new_vhash
        ; key = current_node.content.key
        ; lhash = H.node_hash current_node.content.left
        ; rhash = H.node_hash current_node.content.right
        }
    in
    let n = H.node { current_node.content with value = new_value; vhash = new_vhash } in
    let op =
      Update
        { key = current_node.content.key
        ; old_value = current_node.content.value
        ; new_value
        }
    in
    op, p, Some n
  ;;

  (* Doesn't change anything. 
         This happens when the search reached an empty leaf and the updater returned None so there is nothing to insert. *)
  let noop (key : 'k) (hash : thash) : ('k, 'v) op * 'k proof * ('k, 'v) hnode option =
    Noop { key }, Proof.NoChange hash, None
  ;;

  (* This implements a generic operation on the tree with the given root. It returns the following:
     1. the operation executed
     2. the proof for the above operation
     3. the updated version of the root, if the operation did any updates, or the original root, if nothing has changed
     4. the result of the operation (ex: the found value in case of a lookup)*)
  let process_tree
      (type k v result)
      (handle_key_not_found : unit -> (k, v) op * k proof * (k, v) hnode option * result)
      (handle_key_found :
        (k, v) hnode -> (k, v) op * k proof * (k, v) hnode option * result)
      (key : k)
      (root : (k, v) hnode option)
      : (k, v) op * k proof * (k, v) hnode option * result
    =
    (* Updates a possibly missing node. 
          If the node is empty then we try to insert. If not empty then we try to update its content *)
    let rec update_opt_node (current_node_opt : (k, v) hnode option)
        : (k, v) op * k proof * (k, v) hnode option * result
      =
      current_node_opt
      |> OptionExt.fold_lazy ~none:handle_key_not_found ~some:update_non_empty_node
    (* We reached a situation when the node with the given key was not found. 
          If the updater return a value we need to create a new leaf for it. Otherwise nothing will happen *)
    (* Some value in the subtree starting at current_node needs to be updated. 
          Depending on the key value we will update the current node or the left subtree or the right subtree *)
    and update_non_empty_node (current_node : (k, v) hnode)
        : (k, v) op * k proof * (k, v) hnode option * result
      =
      if current_node.content.key = key
      then handle_key_found current_node
      else if current_node.content.key < key
      then update_right_subtree current_node
      else (*i.e. current_node.key > key *) update_left_subtree current_node
    (* We found the node that needs to be updated. 
          If the updater returns a Some new value then we replace the old value in the node with the new one.
          If the updater returns None then we remove the current_node*)
    (* Go right with the search.*)
    and update_right_subtree (current_node : (k, v) hnode)
        : (k, v) op * k proof * (k, v) hnode option * result
      =
      let op, update_right_proof, new_right_node, result =
        update_opt_node current_node.content.right
      in
      let p =
        Proof.GoRight
          { lhash = H.node_hash current_node.content.left
          ; right_proof = update_right_proof
          ; key = current_node.content.key
          ; vhash = current_node.content.vhash
          }
      in
      let n = H.node { current_node.content with right = new_right_node } in
      op, p, Some n, result
    (* Go left with the search *)
    and update_left_subtree (current_node : (k, v) hnode)
        : (k, v) op * k proof * (k, v) hnode option * result
      =
      let op, update_left_proof, new_left_node, result =
        update_opt_node current_node.content.left
      in
      let p =
        Proof.GoLeft
          { rhash = H.node_hash current_node.content.right
          ; left_proof = update_left_proof
          ; key = current_node.content.key
          ; vhash = current_node.content.vhash
          }
      in
      let n = H.node { current_node.content with left = new_left_node } in
      op, p, Some n, result
    in
    update_opt_node root
  ;;

  (* A convenience function that wraps a process_tree handler that doesn't return anything and only updates the current node *)
  let write_only_handler
      (handler : 'i -> ('k, 'v) op * 'k proof * ('k, 'v) hnode option)
      (input : 'i)
      : ('k, 'v) op * 'k proof * ('k, 'v) hnode option * unit
    =
    let o, p, n = handler input in
    o, p, n, ()
  ;;

  (* A convenience function that wraps a process_tree "key-found" handler that doesn't change the current node but returns a value *)
  let read_only_handle_key_found
      (handler : ('k, 'v) hnode -> ('k, 'v) op * 'k proof * 'r)
      (input : ('k, 'v) hnode)
      : ('k, 'v) op * 'k proof * ('k, 'v) hnode option * 'r
    =
    let o, p, r = handler input in
    o, p, Some input, r
  ;;

  (* A convenience function that wraps a process_tree "key-not-found" handler that doesn't create a new node but returns a value *)
  let read_only_handle_key_not_found (handler : unit -> ('k, 'v) op * 'k proof * 'r) ()
      : ('k, 'v) op * 'k proof * ('k, 'v) hnode option * 'r
    =
    let o, p, r = handler () in
    o, p, None, r
  ;;

  let lookup (key : 'k) (MerkleTreeMap root : ('k, 'v) t)
      : ('k, 'v) op * 'k proof * 'v option
    =
    let op = Lookup { key } in
    let handle_key_not_found () : ('k, 'v) op * 'k proof * 'v option =
      op, Proof.NotFound, None
    in
    let handle_key_found (node : ('k, 'v) hnode) : ('k, 'v) op * 'k proof * 'v option =
      let proof =
        Proof.Found
          { key = node.content.key
          ; lhash = H.node_hash node.content.left
          ; rhash = H.node_hash node.content.right
          ; vhash = node.content.vhash
          }
      in
      op, proof, Some node.content.value
    in
    let op, proof, _new_root, result =
      process_tree
        (read_only_handle_key_not_found handle_key_not_found)
        (read_only_handle_key_found handle_key_found)
        key
        root
    in
    op, proof, result
  ;;

  (* 
      The updater function is called with None when the element is missing or Some v when the v value exists in the map.
      If the updater function returns None for an existing key then the element is removed.
      If the key doesn't exist (i.e. update is called with None) and the 
    *)
  let update_map
      (key : 'k)
      (updater : 'v option -> 'v option)
      (MerkleTreeMap root : ('k, 'v) t)
      : ('k, 'v) op * 'k proof * ('k, 'v) t
    =
    let handle_key_not_found () : ('k, 'v) op * 'k proof * ('k, 'v) hnode option =
      updater None
      |> OptionExt.fold_lazy ~none:(fun () -> noop key H.tempty) ~some:(insert key)
    in
    let handle_key_found (current_node : ('k, 'v) hnode)
        : ('k, 'v) op * 'k proof * ('k, 'v) hnode option
      =
      updater (Some current_node.content.value)
      |> OptionExt.fold_lazy
           ~none:(fun () -> remove_node current_node)
           ~some:(replace_value current_node)
    in
    let op, proof, new_root, _ =
      process_tree
        (write_only_handler handle_key_not_found)
        (write_only_handler handle_key_found)
        key
        root
    in
    op, proof, MerkleTreeMap new_root
  ;;

  let remove (key : 'k) (mtm : ('k, 'v) t) : ('k, 'v) op * 'k proof * ('k, 'v) t =
    update_map key (fun _ -> None) mtm
  ;;

  let upsert (key : 'k) (value : 'v) (mtm : ('k, 'v) t)
      : ('k, 'v) op * 'k proof * ('k, 'v) t
    =
    update_map key (fun _ -> Some value) mtm
  ;;

  (* Verifies that the given op is valid for the given proof and that the given hashes are valid for the given proof*)
  let verify_proof
      (op : ('k, 'v) op)
      (proof : 'k proof)
      (initial_root_hash : hash)
      (final_root_hash : hash)
      : bool
    =
    Proof.matches_op proof op
    && Proof.matches_hashes proof initial_root_hash final_root_hash
  ;;

  let from_list (kvs : ('k * 'v) list) : ('k, 'v) t =
    List.fold_left
      (fun acc (k, v) ->
        let _op, _proof, map = update_map k (CCFun.const @@ Some v) acc in
        map)
      empty
      kvs
  ;;

  let to_list (MerkleTreeMap t : ('k, 'v) t) : ('k * 'v) list =
    let rec go (node_opt : ('k, 'v) hnode option) : ('k * 'v) list =
      node_opt
      |> Option.map (fun node ->
             List.concat
               [ go node.content.left
               ; [ node.content.key, node.content.value ]
               ; go node.content.right
               ])
      |> Option.to_list
      |> List.flatten
    in
    go t
  ;;

  let show
      (kfmt : Format.formatter -> 'k -> unit)
      (vfmt : Format.formatter -> 'v -> unit)
      (MerkleTreeMap t : ('k, 'v) t)
      : string
    =
    String.concat
      ""
      [ "MerkleTreeMap ("
      ; Option.fold ~none:"Empty" ~some:(fun root -> show_hnode kfmt vfmt root) t
      ; ")"
      ]
  ;;
end
