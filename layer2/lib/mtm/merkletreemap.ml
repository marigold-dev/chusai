open Merklemap

(* A map implemented as a Merkle-ized binary search tree *)
module MerkleTreeMap(Hash : Hash_algo.Hash)  : MerkleMap = 
  struct
    type hash = Hash.t
    [@@deriving show]

    module Proof = struct
      type t 
      = GoLeft of {
        left_proof : t;
        khash : hash;
        vhash : hash;
        rhash : hash;
      } 
      | GoRight of {
        right_proof : t;
        khash : hash;
        vhash : hash;
        lhash : hash;
      } 
      | ValueChanged of {
        initial_vhash : hash;
        final_vhash : hash;
        khash : hash;
        lhash:hash;
        rhash:hash;
      }
      | NewLeaf of {
        vhash : hash;
        khash : hash;
      }
      | ReplaceWithLeftChild of {
        initial_khash : hash;
        initial_vhash : hash;
        initial_lhash : hash;
        final_khash : hash;
        final_vhash : hash;
        final_lhash : hash;
        final_rhash : hash;
      }
      | ReplaceWithRightChild of {
        initial_khash : hash;
        initial_vhash : hash;
        initial_rhash : hash;
        final_khash : hash;
        final_vhash : hash;
        final_lhash : hash;
        final_rhash : hash;
      }
      | ReplaceWithSmallestRight of {
        initial_khash : hash;
        initial_vhash : hash;
        final_khash : hash;
        final_vhash : hash;
        lhash : hash;
        replacement_proof_right : t;
      }
      | ReplaceWithBiggestLeft of {
        initial_khash : hash;
        initial_vhash : hash;
        final_khash : hash;
        final_vhash : hash;
        rhash : hash;
        replacement_proof_left : t;
      }
      | RemoveLeaf of hash
      | Noop of hash
      [@@deriving show]
    end

    type ('k, 'v) non_empty_node = {
      key : 'k;
      value : 'v;
      hash : hash;
      khash : hash;
      vhash : hash;
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

    let combine_hashes (lhash : hash) (khash : hash) (vhash : hash) (rhash : hash) = Hash.(lhash ++ khash ++ vhash ++ rhash)

    let empty (kfmt : Format.formatter -> 'k -> unit) (vfmt : Format.formatter -> 'v -> unit) : ('k, 'v) t = 
      {
        kfmt = kfmt;
        vfmt = vfmt;
        root = Empty
      }

    let node_hash (map_root : ('k, 'v) node) : hash = 
      match map_root with
      | Empty -> Hash.empty
      | Node node -> node.hash
      
    let root_hash (map : ('k, 'v) t) : hash =  node_hash map.root

    (* 
      The updater function is called with None when the element is missing or Some v when the v value exists in the map.
      If the updater function returns None for an existing key then the element is removed.
      If the key doesn't exist (i.e. update is called with None) and the 
    *)
    let update_map (key : 'k) (updater : 'v option -> 'v option) (mtm : ('k, 'v) t) : proof * ('k, 'v) t =
      
      let update_not_found (value : 'v) : proof * ('k, 'v) node =
        let khash =  Hash.hash key in
        let vhash = Hash.hash value in
        let p = Proof.NewLeaf {khash = khash; vhash = vhash} in 
        let n = Node {
            key = key; 
            value=value; 
            khash = khash; 
            vhash = vhash; 
            left = Empty; 
            right = Empty;
            hash = combine_hashes Hash.empty khash vhash Hash.empty;
          } in
        p, n
      in

      let rec remove_node (current_node : ('k, 'v) non_empty_node) : proof * ('k, 'v) node =
        let _ = Format.printf "remove_node: current_node=%s\n" @@ show_non_empty_node mtm.kfmt mtm.vfmt current_node in
        match current_node.left, current_node.right with
        | Empty, Empty -> 
          let _ = Format.printf "remove_node: Removing leaf!!!\n" in
          Proof.RemoveLeaf current_node.hash, Empty
        | Empty, Node right_child -> 
          let p = Proof.ReplaceWithRightChild {
            initial_khash = current_node.khash;
            initial_vhash = current_node.vhash;
            initial_rhash = right_child.hash;
            final_khash = right_child.khash;
            final_vhash = right_child.vhash;
            final_lhash = node_hash right_child.left;
            final_rhash = node_hash right_child.right
          } in
          let n = Node right_child in 
          let _ = Format.printf "remove_node: returned node=%s\n" @@ show_node mtm.kfmt mtm.vfmt n in
          p, n
        | Node left_child, Empty -> 
          let p = Proof.ReplaceWithLeftChild {
            initial_khash = current_node.khash;
            initial_vhash = current_node.vhash;
            initial_lhash = left_child.hash;
            final_khash = left_child.khash;
            final_vhash = left_child.vhash;
            final_lhash = node_hash left_child.left;
            final_rhash = node_hash left_child.right
          } in
          let n = Node left_child in 
          let _ = Format.printf "remove_node: returned node=%s\n" @@ show_node mtm.kfmt mtm.vfmt n in
          p, n
        | Node original_left, Node right -> 
          let replacement_proof, replacement_key, replacement_value, new_left = extract_biggest_child original_left in
          let _ = Format.printf "remove_node: new_left=%s\n" @@ show_node mtm.kfmt mtm.vfmt new_left in
          let p = Proof.ReplaceWithBiggestLeft {
            initial_khash = current_node.khash;
            initial_vhash = current_node.vhash;
            final_khash =hash ;
            final_vhash = replacement_vhash;
            rhash = right.hash;
            replacement_proof_left = replacement_proof;
          } in
          let n = Node { original_left with
            left = new_left;
            right = current_node.right;
            hash = combine_hashes (node_hash new_left) original_left.khash original_left.vhash right.hash
          } in 
          let _ = Format.printf "remove_node: returned node=%s\n" @@ show_node mtm.kfmt mtm.vfmt n in
          p, n   
      and extract_biggest_child (current_node : ('k, 'v) non_empty_node) : proof * 'k * 'v * ('k, 'v) node =
          failwith "todo"
      in

      let replace_value (current_node : ('k, 'v) non_empty_node) (new_value : 'v) : proof * ('k, 'v) node =
          let new_vhash = Hash.hash new_value in 
          let p = Proof.ValueChanged {
              initial_vhash = current_node.vhash;
              final_vhash = new_vhash;
              khash = current_node.khash;
              lhash = node_hash current_node.left;
              rhash = node_hash current_node.right;
            } in
          let n = Node { current_node with
            vhash = new_vhash;
            hash = combine_hashes (node_hash current_node.left) current_node.khash new_vhash (node_hash current_node.right)
          } in
          p, n
      in

      let rec update_node (current_node : ('k, 'v) node) : proof * ('k, 'v) node =
        match current_node with
          | Empty -> 
            updater None
            |> Optionext.fold_lazy 
              (fun () -> Proof.Noop Hash.empty, Empty)
              update_not_found
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
            lhash = node_hash current_node.left;
            right_proof = update_right_proof;
            khash = current_node.khash;
            vhash = current_node.vhash;
          }  in

          let n = Node { current_node with
            hash = combine_hashes (node_hash current_node.left) current_node.khash current_node.vhash (node_hash new_right_node);
            right = new_right_node          
          } in
          p, n
        else (*i.e. current_node.key > key *) 
          let update_left_proof, new_left_node = update_node current_node.left in

          let p = Proof.GoLeft {
            rhash = node_hash current_node.left;
            left_proof = update_left_proof;
            khash = current_node.khash;
            vhash = current_node.vhash;
          }  in

          let n = Node { current_node with 
            hash = combine_hashes (node_hash current_node.left) current_node.khash current_node.vhash (node_hash new_left_node);
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
      let rec compute_hashes (current_proof : proof) : hash * hash =
        match current_proof with
        | Proof.NewLeaf p -> Hash.empty, combine_hashes Hash.empty p.khash p.vhash Hash.empty
        | Proof.GoLeft p -> 
            let initial_lhash, final_lhash = compute_hashes p.left_proof in
            let initial = combine_hashes initial_lhash p.khash p.vhash p.rhash in
            let final = combine_hashes final_lhash p.khash p.vhash p.rhash in
            initial, final
        | Proof.GoRight p -> 
          let initial_rhash, final_rhash = compute_hashes p.right_proof in
          let initial = combine_hashes p.lhash p.khash p.vhash initial_rhash in
          let final = combine_hashes p.lhash p.khash p.vhash final_rhash in
          initial, final
        | Proof.ValueChanged p -> 
          let initial = combine_hashes p.lhash p.khash p.initial_vhash p.rhash in
          let final = combine_hashes p.lhash p.khash p.final_vhash p.rhash in
          initial, final
        | Proof.RemoveLeaf initial_hash -> initial_hash, Hash.empty
        | Proof.ReplaceWithSmallestRight p -> 
          let replacement_initial, replacement_final = compute_hashes p.replacement_proof_right in
          let initial = combine_hashes p.lhash p.initial_khash p.initial_vhash replacement_initial in
          let final = combine_hashes p.lhash p.final_khash p.final_vhash replacement_final in
          initial, final
        | Proof.ReplaceWithBiggestLeft p -> 
          let replacement_initial, replacement_final = compute_hashes p.replacement_proof_left in
          let initial = combine_hashes replacement_initial p.initial_khash p.initial_vhash p.rhash in
          let final = combine_hashes replacement_final p.final_khash p.final_vhash p.rhash in
          initial, final
        | Proof.ReplaceWithLeftChild p ->
          let initial = combine_hashes p.initial_lhash p.initial_khash p.initial_vhash Hash.empty in
          let final = combine_hashes p.final_lhash p.final_khash p.final_vhash p.final_rhash in
          initial, final
        | Proof.ReplaceWithRightChild p ->
          let initial = combine_hashes Hash.empty p.initial_khash p.initial_vhash p.initial_rhash in
          let final = combine_hashes p.final_lhash p.final_khash p.final_vhash p.final_rhash in
          initial, final
        | Proof.Noop h -> h, h (* no changes so intial and final hashes are the same *)
          in
      let computed_initial_hash, computed_final_hash = compute_hashes root_proof in
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
