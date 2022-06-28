open Merklemap

(* A map implemented as a Merkle-ized binary search tree *)
module MerkleTreeMap(Hash : Hash_algo.Hash)  : MerkleMap = 
  struct
    type hash = Hash.t
    [@@deriving show]

    type khash = KHash of hash 
      [@@deriving show]

    type vhash = VHash of hash 
      [@@deriving show]

    type thash = THash of hash
      [@@deriving show]

    module Proof = struct
      type 'k t 
      = GoLeft of {
        left_proof : 'k t;
        key : 'k;
        vhash : vhash;
        rhash : thash;
      } 
      | GoRight of {
        right_proof : 'k t;
        key : 'k;
        vhash : vhash;
        lhash : thash;
      } 
      | ValueChanged of {
        initial_vhash : vhash;
        final_vhash : vhash;
        key : 'k;
        lhash:thash;
        rhash:thash;
      }
      | NewLeaf of {
        vhash : vhash;
        key : 'k;
      }
      | ReplaceWithLeftChild of {
        initial_key : 'k;
        initial_vhash : vhash;
        initial_lhash : thash;
        final_key : 'k;
        final_vhash : vhash;
        final_lhash : thash;
        final_rhash : thash;
      }
      | ReplaceWithRightChild of {
        initial_key : 'k;
        initial_vhash : vhash;
        initial_rhash : thash;
        final_key: 'k;
        final_vhash : vhash;
        final_lhash : thash;
        final_rhash : thash;
      }
      | ReplaceWithBiggestLeft of {
        initial_key : 'k;
        initial_vhash : vhash;
        final_key : 'k;
        final_vhash : vhash;
        rhash : thash;
        replacement_proof_left : 'k t;
      }
      | RemoveLeaf of {
        vhash : vhash;
        key : 'k
      }
      | NoChange of thash
      [@@deriving show]
    end

    type ('k, 'v) tnode = {
      key : 'k;
      value : 'v;
      khash : khash;
      vhash : vhash;
      left : ('k, 'v) hnode option;
      right : ('k, 'v) hnode option;
    } [@@deriving show]
    and ('k, 'v) hnode = {
      thash : thash;
      content : ('k, 'v) tnode
    } [@@deriving show]

    type ('k, 'v) t = {
      kfmt : Format.formatter -> 'k -> unit;
      vfmt : Format.formatter -> 'v -> unit;
      root : ('k, 'v) hnode option
    }

    type ('k, 'v) op 
    = Insert of {
      key : 'k; 
      value :'v
      }
    | Update of {
      key : 'k;
      old_value : 'v;
      new_value : 'v
    }
    | Remove of {
      key : 'k
    }
    | Noop of {
      key : 'k
    }
    [@@deriving show]


    type 'k proof = 'k Proof.t

    let empty (kfmt : Format.formatter -> 'k -> unit) (vfmt : Format.formatter -> 'v -> unit) : ('k, 'v) t = 
      {
        kfmt = kfmt;
        vfmt = vfmt;
        root = None
      }
      
    module H = struct 
      let key k = KHash (Hash.hash k)
      let value v = VHash (Hash.hash v)

      
      let combine (THash lhash : thash) (KHash khash : khash) (VHash vhash : vhash) (THash rhash : thash) = 
        let h = Hash.(lhash ++ khash ++ vhash ++ rhash) in
        let _ = Format.printf "combine:%s\n        %s\n        %s\n        %s\n     -> %s\n" (show_hash lhash) (show_hash khash) (show_hash vhash) (show_hash rhash) (show_hash h) in
        THash h

      let node_hash (node_opt : ('k, 'v) hnode option) : thash = 
        node_opt
        |> Option.fold
          ~none: (THash Hash.empty)
          ~some: (fun hnode -> hnode.thash)

      let node (tnode : ('k, 'v) tnode) = 
        { thash = combine (node_hash tnode.left) tnode.khash tnode.vhash (node_hash tnode.right);
          content = tnode;
        }

      let tempty : thash = THash Hash.empty
    end

    let root_hash (map : ('k, 'v) t) : hash =  
      let THash h = H.node_hash map.root in
      h

    (* 
      The updater function is called with None when the element is missing or Some v when the v value exists in the map.
      If the updater function returns None for an existing key then the element is removed.
      If the key doesn't exist (i.e. update is called with None) and the 
    *)
    let update_map (key : 'k) (updater : 'v option -> 'v option) (mtm : ('k, 'v) t) : ('k, 'v) op * 'k proof * ('k, 'v) t =
      
      let insert (value : 'v) : ('k, 'v) op * 'k proof * ('k, 'v) hnode option =
        let khash =  H.key key in
        let vhash = H.value value in
        let p = Proof.NewLeaf {key = key; vhash = vhash} in 
        let n = H.node {
            key = key; 
            value=value; 
            khash = khash; 
            vhash = vhash; 
            left = None; 
            right = None;
          } in
        let _ = Format.printf "insert: n=%s\np=%s\n" (show_hnode mtm.kfmt mtm.vfmt n) (Proof.show mtm.kfmt p) in
        Insert {key = key; value = value}, p, Some n
      in

      (* Returns the k,v of the biggest child in the subtree starting at current_node, the proof and the subtree without the biggest child *)
      let rec extract_biggest_child (current_node : ('k, 'v) hnode) : 'k proof * 'k * 'v * ('k, 'v) hnode option =
        match current_node.content.left, current_node.content.right with 
        | None, None -> 
          Proof.RemoveLeaf {key = current_node.content.key; vhash = current_node.content.vhash}, current_node.content.key, current_node.content.value, None 
        | Some left, None ->
          let proof = Proof.ReplaceWithLeftChild {
            initial_key = current_node.content.key;
            initial_vhash = current_node.content.vhash;
            initial_lhash = H.node_hash current_node.content.left;
            final_key = left.content.key;
            final_vhash = left.content.vhash;
            final_lhash = H.node_hash left.content.left;
            final_rhash = H.node_hash left.content.right;       
          } in
          proof, current_node.content.key, current_node.content.value, current_node.content.left
        | _, Some right ->
          let child_proof, biggest_key, biggest_value, right_wihout_biggest_child = extract_biggest_child right in
          let proof = Proof.GoRight {
            right_proof = child_proof;
            key = current_node.content.key;
            vhash = current_node.content.vhash;
            lhash = H.node_hash current_node.content.left;
          } in
          let current_without_biggest_child = H.node { current_node.content with 
            right = right_wihout_biggest_child;
          } in
          proof, biggest_key, biggest_value, Some current_without_biggest_child
        in

      let rec remove_node (current_node : ('k, 'v) hnode) : ('k, 'v) op * 'k proof * ('k, 'v) hnode option =
        let _ = Format.printf "remove_node: current_node=%s\n" @@ show_hnode mtm.kfmt mtm.vfmt current_node in

        let proof, node = 
          match current_node.content.left, current_node.content.right with
          | None, None -> 
            let _ = Format.printf "remove_node: Removing leaf!!!\n" in
            Proof.RemoveLeaf {vhash = current_node.content.vhash; key = current_node.content.key}, None
          | None, Some right_child -> 
            let p = Proof.ReplaceWithRightChild {
              initial_key = current_node.content.key;
              initial_vhash = current_node.content.vhash;
              initial_rhash = right_child.thash;
              final_key = right_child.content.key;
              final_vhash = right_child.content.vhash;
              final_lhash = H.node_hash right_child.content.left;
              final_rhash = H.node_hash right_child.content.right
            } in
            let n = H.node right_child.content in 
            let _ = Format.printf "remove_node: returned node=%s\n" @@ show_hnode mtm.kfmt mtm.vfmt n in
            p, Some n
          | Some left_child, None -> 
            let p = Proof.ReplaceWithLeftChild {
              initial_key = current_node.content.key;
              initial_vhash = current_node.content.vhash;
              initial_lhash = left_child.thash;
              final_key = left_child.content.key;
              final_vhash = left_child.content.vhash;
              final_lhash = H.node_hash left_child.content.left;
              final_rhash = H.node_hash left_child.content.right
            } in
            let _ = Format.printf "remove_node: returned node=%s\n" @@ show_hnode mtm.kfmt mtm.vfmt left_child in
            p, Some left_child
          | Some original_left, Some right -> 
            let replacement_proof, replacement_key, replacement_value, new_left = extract_biggest_child original_left in
            let _ = Format.printf "after extract biggest child\nproof=%s\nnew_left=%s\n" 
              (Proof.show mtm.kfmt replacement_proof) (Option.fold ~none:"None" ~some:(show_hnode mtm.kfmt mtm.vfmt) new_left) in
            let _ = Debug.print "key=" replacement_key in
            let _ = Debug.print "value=" replacement_value in
            let replacement_vhash = H.value replacement_value in
            let p = Proof.ReplaceWithBiggestLeft {
              initial_key = current_node.content.key;
              initial_vhash = current_node.content.vhash;
              final_key = replacement_key ;
              final_vhash = replacement_vhash;
              rhash = right.thash;
              replacement_proof_left = replacement_proof;
            } in
            let n = H.node {
              key = replacement_key;
              value = replacement_value;
              khash = H.key replacement_key;
              vhash = replacement_vhash;
              left = new_left;
              right = current_node.content.right;
            } in 
            let _ = Format.printf "remove_node: returned node=%s\n" @@ show_hnode mtm.kfmt mtm.vfmt n in
            p, Some n
          in
          Remove {key = current_node.content.key}, proof, node

      in

      let replace_value (current_node : ('k, 'v) hnode) (new_value : 'v) : ('k, 'v) op * 'k proof * ('k, 'v) hnode option =
          let new_vhash = H.value new_value in 
          let p = Proof.ValueChanged {
              initial_vhash = current_node.content.vhash;
              final_vhash = new_vhash;
              key = current_node.content.key;
              lhash = H.node_hash current_node.content.left;
              rhash = H.node_hash current_node.content.right;
            } in
          let n = H.node { current_node.content with
            vhash = new_vhash;
          } in
          let op = Update {key = current_node.content.key; old_value = current_node.content.value; new_value = new_value} in 
          op, p, Some n
      in

      let rec update_node (current_node_opt : ('k, 'v) hnode option) : ('k, 'v) op * 'k proof * ('k, 'v) hnode option =
        current_node_opt
        |> Option.fold
          ~none:(
            updater None
            |> Optionext.fold_lazy 
              (fun () -> Noop {key = key}, Proof.NoChange H.tempty, None)
              insert)
          ~some:(fun current_node -> update_non_empty_node current_node)

      and update_non_empty_node (current_node : ('k, 'v) hnode) : ('k, 'v) op * 'k proof * ('k, 'v) hnode option =
        if current_node.content.key =  key then 
          updater (Some current_node.content.value)
          |> Optionext.fold_lazy
            (fun () -> remove_node current_node)
            (replace_value current_node)
              
        else if current_node.content.key < key then
          let op, update_right_proof, new_right_node = update_node current_node.content.right in

          let p = Proof.GoRight {
            lhash = H.node_hash current_node.content.left;
            right_proof = update_right_proof;
            key = current_node.content.key;
            vhash = current_node.content.vhash;
          }  in

          let n = H.node { current_node.content with
            right = new_right_node          
          } in
          op, p, Some n

        else (*i.e. current_node.key > key *) 
          let op, update_left_proof, new_left_node = update_node current_node.content.left in

          let p = Proof.GoLeft {
            rhash = H.node_hash current_node.content.right;
            left_proof = update_left_proof;
            key = current_node.content.key;
            vhash = current_node.content.vhash;
          }  in

          let n = H.node { current_node.content with 
            left = new_left_node        
          } in
          op, p, Some n
      in

      let op, proof, new_root = update_node mtm.root in
      op, proof, { mtm with root = new_root}

    let remove (key : 'k) (mtm : ('k, 'v) t) : ('k, 'v) op * 'k proof * ('k, 'v) t =
      update_map key (fun _ -> None) mtm

    let upsert (key : 'k) (value: 'v) (mtm : ('k, 'v) t) : ('k, 'v) op * 'k proof * ('k, 'v) t =
      update_map key (fun _ -> Some value) mtm

    let verify_proof  (kfmt : Format.formatter -> 'k -> unit) (vfmt : Format.formatter -> 'v -> unit) (op : ('k, 'v) op) (proof : 'k proof) (initial_root_hash : hash) (final_root_hash : hash) : bool =
    let _ = Format.printf "verify_proof: op=%s\nproof=%s\n" (show_op kfmt vfmt op) (Proof.show kfmt proof) in
    let proof_matches_hashes() =
        let rec compute_hashes (current_proof : 'k proof) : thash * thash =
          let initial, final = match current_proof with
          | Proof.NewLeaf p -> H.tempty, H.combine H.tempty (H.key p.key) p.vhash H.tempty
          | Proof.GoLeft p -> 
              let initial_lhash, final_lhash = compute_hashes p.left_proof in
              let khash =  H.key p.key in 
              let initial = H.combine initial_lhash khash p.vhash p.rhash in
              let final = H.combine final_lhash khash p.vhash p.rhash in
              initial, final
          | Proof.GoRight p -> 
            let initial_rhash, final_rhash = compute_hashes p.right_proof in
            let khash =  H.key p.key in 
            let initial = H.combine p.lhash khash p.vhash initial_rhash in
            let final = H.combine p.lhash khash p.vhash final_rhash in
            initial, final
          | Proof.ValueChanged p -> 
            let khash =  H.key p.key in 
            let initial = H.combine p.lhash khash p.initial_vhash p.rhash in
            let final = H.combine p.lhash khash p.final_vhash p.rhash in
            initial, final
          | Proof.RemoveLeaf p -> H.combine H.tempty (H.key p.key) p.vhash H.tempty, H.tempty
          | Proof.ReplaceWithLeftChild p ->
            let initial = H.combine p.initial_lhash (H.key p.initial_key) p.initial_vhash H.tempty in
            let final = H.combine p.final_lhash (H.key p.final_key) p.final_vhash p.final_rhash in
            initial, final
          | Proof.ReplaceWithRightChild p ->
            let initial = H.combine H.tempty (H.key p.initial_key)  p.initial_vhash p.initial_rhash in
            let final = H.combine p.final_lhash (H.key p.final_key) p.final_vhash p.final_rhash in
            initial, final
          | Proof.ReplaceWithBiggestLeft p -> 
            let replacement_initial, replacement_final = compute_hashes p.replacement_proof_left in
            let initial = H.combine replacement_initial (H.key p.initial_key) p.initial_vhash p.rhash in
            let final = H.combine replacement_final (H.key p.final_key) p.final_vhash p.rhash in
            initial, final
          | Proof.NoChange h -> h, h (* no changes so intial and final hashes are the same *)
            in
          initial, final
          in

        let THash computed_initial_hash, THash computed_final_hash = compute_hashes proof in
        let _ = Format.printf "initial_hash=%s\ncomputed_initial_hash=%s\nfinal_hash=%s\ncomputed_final_hash=%s\n" 
          (show_hash initial_root_hash) (show_hash computed_initial_hash) (show_hash final_root_hash) (show_hash computed_final_hash) in 
        computed_initial_hash = initial_root_hash && computed_final_hash = final_root_hash
        in

      let op_matches_proof() : bool = 
        match op with
        | Noop noop -> 
          let rec is_valid_noop = function
           | Proof.NoChange _ -> true 
           | Proof.GoLeft go_left -> go_left.key > noop.key && is_valid_noop go_left.left_proof
           | Proof.GoRight go_right -> go_right.key < noop.key && is_valid_noop go_right.right_proof
           | _ -> false
        in 
        is_valid_noop proof
        | Insert insert_op -> 
          let rec is_valid_insert = function
            | Proof.GoLeft go_left -> go_left.key > insert_op.key && is_valid_insert go_left.left_proof
            | Proof.GoRight go_right -> go_right.key < insert_op.key && is_valid_insert go_right.right_proof
            | Proof.NewLeaf new_leaf -> new_leaf.key = insert_op.key && new_leaf.vhash = (H.value insert_op.value)
            | _ -> false
          in
          is_valid_insert proof
        | Update update_op ->
          let rec is_valid_update = function
          | Proof.GoLeft go_left -> go_left.key > update_op.key && is_valid_update go_left.left_proof
          | Proof.GoRight go_right -> go_right.key < update_op.key && is_valid_update go_right.right_proof
          | Proof.ValueChanged updated_node -> 
            updated_node.key = update_op.key && 
            updated_node.final_vhash = (H.value update_op.new_value) && 
            updated_node.initial_vhash = (H.value update_op.old_value)
          in 
          is_valid_update proof
        | Remove remove_op -> 
          let rec is_valid_remove = function
          | Proof.GoLeft go_left -> go_left.key > remove_op.key && is_valid_remove go_left.left_proof
          | Proof.GoRight go_right -> go_right.key < remove_op.key && is_valid_remove go_right.right_proof
          | Proof.RemoveLeaf remove_leaf -> remove_leaf.key = remove_op.key
          | Proof.ReplaceWithBiggestLeft replace -> replace.initial_key = remove_op.key && replace.final_key < remove_op.key
          | Proof.ReplaceWithLeftChild replace -> replace.initial_key = remove_op.key && replace.final_key < remove_op.key
          | Proof.ReplaceWithRightChild replace -> replace.initial_key = remove_op.key && replace.final_key > remove_op.key
          in
          is_valid_remove proof
        in
        
      let _ = Format.printf "proof matches hashes: %B\nop matches proof: %B" (proof_matches_hashes ()) (op_matches_proof ()) in
      op_matches_proof () && proof_matches_hashes ()
          
    let from_list (kfmt : Format.formatter -> 'k -> unit) (vfmt : Format.formatter -> 'v -> unit) (kvs : ('k * 'v) list) : ('k, 'v) t =
      List.fold_left
        ( fun acc (k, v) -> 
          let _op, _proof, map = update_map k (CCFun.const @@ Some v) acc in 
          map)
        (empty kfmt vfmt)
        kvs 

    let to_list (t : ('k, 'v) t) : ('k * 'v) list =
      let rec go (node_opt : ('k, 'v) hnode option) : ('k * 'v) list =
        node_opt
        |> Option.map (fun node ->
            List.concat 
              [ go node.content.left
              ; [(node.content.key, node.content.value)]
              ; go node.content.right
              ])
        |> Option.to_list
        |> List.flatten in
      go t.root

    let show (t : ('k, 'v) t): string =
      String.concat "" [
        "MerkleTreeMap (";
          Option.fold ~none: "Empty" ~some: (fun root -> show_hnode t.kfmt t.vfmt root) t.root;
        ")"
      ]
  end
