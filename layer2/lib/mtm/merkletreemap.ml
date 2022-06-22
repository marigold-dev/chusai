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
      type t 
      = GoLeft of {
        left_proof : t;
        khash : khash;
        vhash : vhash;
        rhash : thash;
      } 
      | GoRight of {
        right_proof : t;
        khash : khash;
        vhash : vhash;
        lhash : thash;
      } 
      | ValueChanged of {
        initial_vhash : vhash;
        final_vhash : vhash;
        khash : khash;
        lhash:thash;
        rhash:thash;
      }
      | NewLeaf of {
        vhash : vhash;
        khash : khash;
      }
      | ReplaceWithLeftChild of {
        initial_khash : khash;
        initial_vhash : vhash;
        initial_lhash : thash;
        final_khash : khash;
        final_vhash : vhash;
        final_lhash : thash;
        final_rhash : thash;
      }
      | ReplaceWithRightChild of {
        initial_khash : khash;
        initial_vhash : vhash;
        initial_rhash : thash;
        final_khash : khash;
        final_vhash : vhash;
        final_lhash : thash;
        final_rhash : thash;
      }
      | ReplaceWithBiggestLeft of {
        initial_khash : khash;
        initial_vhash : vhash;
        final_khash : khash;
        final_vhash : vhash;
        rhash : thash;
        replacement_proof_left : t;
      }
      | RemoveLeaf of thash
      | Noop of thash
      [@@deriving show]
    end

    type ('k, 'v) non_empty_node = {
      key : 'k;
      value : 'v;
      thash : thash;
      khash : khash;
      vhash : vhash;
      left : ('k, 'v) node;
      right : ('k, 'v) node;
    } [@@deriving show]
    and ('k, 'v) node
    = Empty 
    | Node  of ('k, 'v) non_empty_node [@@deriving show]

    type ('k, 'v) t = {
      kfmt : Format.formatter -> 'k -> unit;
      vfmt : Format.formatter -> 'v -> unit;
      root : ('k, 'v) node
    }

    type proof = Proof.t

    let empty (kfmt : Format.formatter -> 'k -> unit) (vfmt : Format.formatter -> 'v -> unit) : ('k, 'v) t = 
      {
        kfmt = kfmt;
        vfmt = vfmt;
        root = Empty
      }
      
    module H = struct 
      let key k = KHash (Hash.hash k)
      let value v = VHash (Hash.hash v)

      
      let combine (THash lhash : thash) (KHash khash : khash) (VHash vhash : vhash) (THash rhash : thash) = 
        let h = Hash.(lhash ++ khash ++ vhash ++ rhash) in
        let _ = Format.printf "combine:%s\n        %s\n        %s\n        %s\n     -> %s\n" (show_hash lhash) (show_hash khash) (show_hash vhash) (show_hash rhash) (show_hash h) in
        THash h

      let node (map_root : ('k, 'v) node) : thash = 
        match map_root with
        | Empty -> THash Hash.empty
        | Node node -> node.thash

      let tempty : thash = THash Hash.empty
    end

    let root_hash (map : ('k, 'v) t) : hash =  
      let THash h = H.node map.root in
      h

    (* 
      The updater function is called with None when the element is missing or Some v when the v value exists in the map.
      If the updater function returns None for an existing key then the element is removed.
      If the key doesn't exist (i.e. update is called with None) and the 
    *)
    let update_map (key : 'k) (updater : 'v option -> 'v option) (mtm : ('k, 'v) t) : proof * ('k, 'v) t =
      
      let insert (value : 'v) : proof * ('k, 'v) node =
        let khash =  H.key key in
        let vhash = H.value value in
        let p = Proof.NewLeaf {khash = khash; vhash = vhash} in 
        let n = Node {
            key = key; 
            value=value; 
            khash = khash; 
            vhash = vhash; 
            left = Empty; 
            right = Empty;
            thash = H.combine H.tempty khash vhash H.tempty;
          } in
        let _ = Format.printf "insert: n=%s\np=%s\n" (show_node mtm.kfmt mtm.vfmt n) (Proof.show p) in
        p, n
      in

      (* Returns the k,v of the biggest child in the subtree starting at current_node, the proof and the subtree without the biggest child *)
      let rec extract_biggest_child (current_node : ('k, 'v) non_empty_node) : proof * 'k * 'v * ('k, 'v) node =
        match current_node.left, current_node.right with 
        | Empty, Empty -> 
          Proof.RemoveLeaf current_node.thash, current_node.key, current_node.value, Empty 
        | Node left, Empty ->
          let proof = Proof.ReplaceWithLeftChild {
            initial_khash = current_node.khash;
            initial_vhash = current_node.vhash;
            initial_lhash = H.node current_node.left;
            final_khash = left.khash;
            final_vhash = left.vhash;
            final_lhash = H.node left.left;
            final_rhash = H.node left.right;       
          } in
          proof, current_node.key, current_node.value, current_node.left
        | _, Node right ->
          let child_proof, biggest_key, biggest_value, right_wihout_biggest_child = extract_biggest_child right in
          let proof = Proof.GoRight {
            right_proof = child_proof;
            khash = current_node.khash;
            vhash = current_node.vhash;
            lhash = H.node current_node.left;
          } in
          let current_without_biggest_child = Node { current_node with 
            right = right_wihout_biggest_child;
            thash = H.combine (H.node current_node.left) current_node.khash current_node.vhash (H.node right_wihout_biggest_child)
          } in
          proof, biggest_key, biggest_value, current_without_biggest_child
        in

      let rec remove_node (current_node : ('k, 'v) non_empty_node) : proof * ('k, 'v) node =
        let _ = Format.printf "remove_node: current_node=%s\n" @@ show_non_empty_node mtm.kfmt mtm.vfmt current_node in
        match current_node.left, current_node.right with
        | Empty, Empty -> 
          let _ = Format.printf "remove_node: Removing leaf!!!\n" in
          Proof.RemoveLeaf current_node.thash, Empty
        | Empty, Node right_child -> 
          let p = Proof.ReplaceWithRightChild {
            initial_khash = current_node.khash;
            initial_vhash = current_node.vhash;
            initial_rhash = right_child.thash;
            final_khash = right_child.khash;
            final_vhash = right_child.vhash;
            final_lhash = H.node right_child.left;
            final_rhash = H.node right_child.right
          } in
          let n = Node right_child in 
          let _ = Format.printf "remove_node: returned node=%s\n" @@ show_node mtm.kfmt mtm.vfmt n in
          p, n
        | Node left_child, Empty -> 
          let p = Proof.ReplaceWithLeftChild {
            initial_khash = current_node.khash;
            initial_vhash = current_node.vhash;
            initial_lhash = left_child.thash;
            final_khash = left_child.khash;
            final_vhash = left_child.vhash;
            final_lhash = H.node left_child.left;
            final_rhash = H.node left_child.right
          } in
          let n = Node left_child in 
          let _ = Format.printf "remove_node: returned node=%s\n" @@ show_node mtm.kfmt mtm.vfmt n in
          p, n
        | Node original_left, Node right -> 
          let replacement_proof, replacement_key, replacement_value, new_left = extract_biggest_child original_left in
          let _ = Format.printf "after extract biggest child\nproof=%s\nnew_left=%s\n" 
            (Proof.show replacement_proof) (show_node mtm.kfmt mtm.vfmt new_left) in
          let _ = Debug.print "key=" replacement_key in
          let _ = Debug.print "value=" replacement_value in
          let _ = Format.printf "remove_node: new_left=%s\n" @@ show_node mtm.kfmt mtm.vfmt new_left in
          let p = Proof.ReplaceWithBiggestLeft {
            initial_khash = current_node.khash;
            initial_vhash = current_node.vhash;
            final_khash = H.key replacement_key ;
            final_vhash = H.value replacement_value;
            rhash = right.thash;
            replacement_proof_left = replacement_proof;
          } in
          let replacement_khash = H.key replacement_key in
          let replacement_vhash = H.value replacement_value in
          let n = Node {
            key = replacement_key;
            value = replacement_value;
            khash = replacement_khash;
            vhash = replacement_vhash;
            left = new_left;
            right = current_node.right;
            thash = H.combine (H.node new_left) replacement_khash replacement_vhash right.thash
          } in 
          let _ = Format.printf "remove_node: returned node=%s\n" @@ show_node mtm.kfmt mtm.vfmt n in
          p, n   

      in

      let replace_value (current_node : ('k, 'v) non_empty_node) (new_value : 'v) : proof * ('k, 'v) node =
          let new_vhash = H.value new_value in 
          let p = Proof.ValueChanged {
              initial_vhash = current_node.vhash;
              final_vhash = new_vhash;
              khash = current_node.khash;
              lhash = H.node current_node.left;
              rhash = H.node current_node.right;
            } in
          let n = Node { current_node with
            vhash = new_vhash;
            thash = H.combine (H.node current_node.left) current_node.khash new_vhash (H.node current_node.right)
          } in
          p, n
      in

      let rec update_node (current_node : ('k, 'v) node) : proof * ('k, 'v) node =
        match current_node with
          | Empty -> 
            updater None
            |> Optionext.fold_lazy 
              (fun () -> Proof.Noop H.tempty, Empty)
              insert
          | Node non_empty_node -> update_non_empty_node non_empty_node

      and update_non_empty_node (current_node : ('k, 'v) non_empty_node) : proof * ('k, 'v) node =
        if current_node.key =  key then 
          updater (Some current_node.value)
          |> Optionext.fold_lazy
            (fun () -> remove_node current_node)
            (replace_value current_node)
              
        else if current_node.key < key then
          let update_right_proof, new_right_node = update_node current_node.right in

          let p = Proof.GoRight {
            lhash = H.node current_node.left;
            right_proof = update_right_proof;
            khash = current_node.khash;
            vhash = current_node.vhash;
          }  in

          let n = Node { current_node with
            thash = H.combine (H.node current_node.left) current_node.khash current_node.vhash (H.node new_right_node);
            right = new_right_node          
          } in
          p, n
        else (*i.e. current_node.key > key *) 
          let update_left_proof, new_left_node = update_node current_node.left in

          let p = Proof.GoLeft {
            rhash = H.node current_node.right;
            left_proof = update_left_proof;
            khash = current_node.khash;
            vhash = current_node.vhash;
          }  in

          let n = Node { current_node with 
            thash = H.combine (H.node new_left_node) current_node.khash current_node.vhash (H.node current_node.right);
            left = new_left_node        
          } in
          p, n
      in

      let proof, new_root = update_node mtm.root in
      proof, { mtm with root = new_root}

    let remove (key : 'k) (mtm : ('k, 'v) t) : proof * ('k, 'v) t =
      update_map key (fun _ -> None) mtm

    let upsert (key : 'k) (value: 'v) (mtm : ('k, 'v) t) : proof * ('k, 'v) t =
      update_map key (fun _ -> Some value) mtm

    let verify_proof (root_proof : proof) (initial_root_hash : hash) (final_root_hash : hash): bool =
      let _ = Format.printf "root_proof=%s\n" @@ Proof.show root_proof in
      let _ = Format.printf "empty_hash=%s\n" @@ show_thash H.tempty in
      let rec compute_hashes (current_proof : proof) : thash * thash =
        let initial, final = match current_proof with
        | Proof.NewLeaf p -> H.tempty, H.combine H.tempty p.khash p.vhash H.tempty
        | Proof.GoLeft p -> 
            let initial_lhash, final_lhash = compute_hashes p.left_proof in
            let initial = H.combine initial_lhash p.khash p.vhash p.rhash in
            let final = H.combine final_lhash p.khash p.vhash p.rhash in
            initial, final
        | Proof.GoRight p -> 
          let initial_rhash, final_rhash = compute_hashes p.right_proof in
          let initial = H.combine p.lhash p.khash p.vhash initial_rhash in
          let final = H.combine p.lhash p.khash p.vhash final_rhash in
          initial, final
        | Proof.ValueChanged p -> 
          let initial = H.combine p.lhash p.khash p.initial_vhash p.rhash in
          let final = H.combine p.lhash p.khash p.final_vhash p.rhash in
          initial, final
        | Proof.RemoveLeaf initial_hash -> initial_hash, H.tempty
        | Proof.ReplaceWithLeftChild p ->
          let initial = H.combine p.initial_lhash p.initial_khash p.initial_vhash H.tempty in
          let final = H.combine p.final_lhash p.final_khash p.final_vhash p.final_rhash in
          initial, final
        | Proof.ReplaceWithRightChild p ->
          let initial = H.combine H.tempty p.initial_khash p.initial_vhash p.initial_rhash in
          let final = H.combine p.final_lhash p.final_khash p.final_vhash p.final_rhash in
          initial, final
        | Proof.ReplaceWithBiggestLeft p -> 
          let replacement_initial, replacement_final = compute_hashes p.replacement_proof_left in
          let initial = H.combine replacement_initial p.initial_khash p.initial_vhash p.rhash in
          let final = H.combine replacement_final p.final_khash p.final_vhash p.rhash in
          initial, final
        | Proof.Noop h -> h, h (* no changes so intial and final hashes are the same *)
          in
        
        let _ = Format.printf "compute_hashes: current_proof=%s\ninitial=%s\n  final=%s\n" (Proof.show current_proof ) (show_thash initial) (show_thash final) in
        initial, final
        in

      let THash computed_initial_hash, THash computed_final_hash = compute_hashes root_proof in
      let _ = Format.printf "initial_hash=%s\ncomputed_initial_hash=%s\nfinal_hash=%s\ncomputed_final_hash=%s\n" 
        (show_hash initial_root_hash) (show_hash computed_initial_hash) (show_hash final_root_hash) (show_hash computed_final_hash) in 
      computed_initial_hash = initial_root_hash && computed_final_hash = final_root_hash
        
    let from_list (kfmt : Format.formatter -> 'k -> unit) (vfmt : Format.formatter -> 'v -> unit) (kvs : ('k * 'v) list) : ('k, 'v) t =
      List.fold_left
        ( fun acc (k, v) -> snd @@ update_map k (CCFun.const @@ Some v) acc)
        (empty kfmt vfmt)
        kvs 

    let to_list (t : ('k, 'v) t) : ('k * 'v) list =
      let rec go (node : ('k, 'v) node) : ('k * 'v) list =
        match node with
        | Empty -> []
        | Node node ->
            List.concat 
              [ go node.left
              ; [(node.key, node.value)]
              ; go node.right
              ] in
      go t.root

    let show (t : ('k, 'v) t): string =
      String.concat "" [
        "MerkleTreeMap (";
          show_node t.kfmt t.vfmt t.root;
        ")"
      ]
  end
