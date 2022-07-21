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

  type 'k proof_step =
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
    | NotFound 
    | Found of
        { key : 'k
        ; vhash : vhash
        ; lhash : thash
        ; rhash : thash
        }
    [@@deriving show]

  type 'k proof = 'k proof_step list

  type ('k, 'v) t = MerkleTreeMap of ('k, 'v) hnode option

  let empty : ('k, 'v) t = MerkleTreeMap None

  let root_hash (MerkleTreeMap map : ('k, 'v) t) : hash =
    let (THash h) = H.node_hash map in
    h
  ;;

  let lookup (type k v) (key : k) (MerkleTreeMap root : (k, v) t)
      : v option * k proof
    =
    (* Updates a possibly missing node. 
          If the node is empty then we try to insert. If not empty then we try to update its content *)
    let rec search_opt_node (current_node_opt : (k, v) hnode option)
        : v option * k proof
      =
      current_node_opt
      |> Option.fold
        ~none: (None,[NotFound] )
        ~some: (fun node -> search_non_empty_node node)
    (* We reached a situation when the node with the given key was not found. 
          If the updater return a value we need to create a new leaf for it. Otherwise nothing will happen *)
    (* Some value in the subtree starting at current_node needs to be updated. 
          Depending on the key value we will update the current node or the left subtree or the right subtree *)
    and search_non_empty_node (current_node : (k, v) hnode)
        : v option * k proof
      =
      if current_node.content.key = key
      then (
        let proof = [Found
        { key = current_node.content.key
        ; lhash = H.node_hash current_node.content.left
        ; rhash = H.node_hash current_node.content.right
        ; vhash = current_node.content.vhash
        }] in
        Some current_node.content.value, proof)
      else if current_node.content.key < key
      then search_right_subtree current_node
      else search_left_subtree current_node
    (* We found the node that needs to be updated. 
          If the updater returns a Some new value then we replace the old value in the node with the new one.
          If the updater returns None then we remove the current_node*)
    (* Go right with the search.*)
    and search_right_subtree (current_node : (k, v) hnode)
        : v option * k proof
      =
      let result, proof_tail = search_opt_node current_node.content.right
      in
      let proof_step =
        GoRight
          { lhash = H.node_hash current_node.content.left
          ; key = current_node.content.key
          ; vhash = current_node.content.vhash
          }
      in
      let proof = proof_step :: proof_tail in
      result, proof
    (* Go left with the search *)
    and search_left_subtree (current_node : (k, v) hnode)
        : v option * k proof
      =
      let result, proof_tail = search_opt_node current_node.content.left
      in
      let proof_step =
        GoLeft
          { rhash = H.node_hash current_node.content.right
          ; key = current_node.content.key
          ; vhash = current_node.content.vhash
          }
      in
      let proof = proof_step :: proof_tail in
      result, proof
      in
    search_opt_node root
  ;;

  let upsert (type k v) (key : k) (value : v) (MerkleTreeMap root : (k, v) t)
      : v option * (k, v) t
    =

    let insert (key : k) (value : v) : (k, v) hnode option =
      let khash = H.key key in
      let vhash = H.value value in
      let n = H.node { key; value; khash; vhash; left = None; right = None } in
      Some n
    in
    (* Replaces the value in the current_node with the new_value. 
        A new node is always returned.*)
    let update (current_node : (k, v) hnode) (new_value : v)
        : (k, v) hnode option
      =
      let new_vhash = H.value new_value in
      let n = H.node { current_node.content with value = new_value; vhash = new_vhash } in
      Some n
    in
    
    (* Updates a possibly missing node. 
        If the node is empty then we try to insert. If not empty then we try to update its content *)
    let rec update_opt_node (current_node_opt : (k, v) hnode option)
        : v option * (k, v) hnode option
      =
      match current_node_opt with 
      | None ->
        let new_node = insert key value in
        None, new_node
      | Some node ->
        let value, new_node = update_non_empty_node node in
        value, new_node
    (* We reached a situation when the node with the given key was not found. 
          If the updater return a value we need to create a new leaf for it. Otherwise nothing will happen *)
    (* Some value in the subtree starting at current_node needs to be updated. 
          Depending on the key value we will update the current node or the left subtree or the right subtree *)
    and update_non_empty_node (current_node : (k, v) hnode)
        : v option * (k, v) hnode option
      =
      if current_node.content.key = key
      then (
        let new_node = update current_node value in
        Some current_node.content.value, new_node)
      else if current_node.content.key < key
      then update_right_subtree current_node
      else (*i.e. current_node.key > key *) update_left_subtree current_node
    (* We found the node that needs to be updated. 
          If the updater returns a Some new value then we replace the old value in the node with the new one.
          If the updater returns None then we remove the current_node*)
    (* Go right with the search.*)
    and update_right_subtree (current_node : (k, v) hnode)
        : v option * (k, v) hnode option
      =
      let result, new_right_node =
        update_opt_node current_node.content.right
      in
      let n = H.node { current_node.content with right = new_right_node } in
      result, Some n
    (* Go left with the search *)
    and update_left_subtree (current_node : (k, v) hnode)
        : v option * (k, v) hnode option
      =
      let result, new_left_node =
        update_opt_node current_node.content.left
      in
      let n = H.node { current_node.content with left = new_left_node } in
      result, Some n
    in
    let result, new_root = update_opt_node root in
    result, MerkleTreeMap new_root
  ;;


  (** Computes he root hash from the proof *)
  let verify_proof (type k) (proof : k proof) (hash : hash)
      : bool
    =
    let compute_hash (step : k proof_step) (acc_opt : thash option)
        : thash option
      =
      match acc_opt, step with
      | Some acc, GoLeft p ->
        let khash = H.key p.key in
        Some (H.combine acc khash p.vhash p.rhash)
      | Some acc, GoRight p ->
        let khash = H.key p.key in
        Some (H.combine p.lhash khash p.vhash acc)
      | None, NotFound -> 
        Some H.tempty
      | None, Found p ->
        let khash = H.key p.key in
        Some (H.combine p.lhash khash p.vhash p.rhash)
      | _ -> None
    in
    List.fold_right compute_hash proof None
    |> Option.fold
      ~some:(function (THash h) -> h = hash)
      ~none:false
  ;;

  let of_seq (type k v) (kvs : (k * v) Seq.t) : (k, v) t =
    Seq.fold_left
      (fun acc (key, value) ->
        let _, map = upsert key value acc in
        map)
      empty
      kvs
  ;;

  let to_seq (type k v) (MerkleTreeMap t : (k, v) t) : (k * v) Seq.t =
    let rec go (node_opt : (k, v) hnode option) : (k * v) Seq.t =
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
      (type k v)
      (kfmt : Format.formatter -> k -> unit)
      (vfmt : Format.formatter -> v -> unit)
      (MerkleTreeMap t : (k, v) t)
      : string
    =
    "MerkleTreeMap ("
    ^ Option.fold ~none:"Empty" ~some:(fun root -> show_hnode kfmt vfmt root) t
    ^ ")"
  ;;

  let pp_proof 
    (type k) 
    (kfmt : Format.formatter -> k -> unit)
    (proof : k proof) 
    : string 
  =
    let steps_strs = List.map (show_proof_step kfmt) proof in
    "Proof [" ^ String.concat ";\n" steps_strs ^ "]"
  ;;
end
