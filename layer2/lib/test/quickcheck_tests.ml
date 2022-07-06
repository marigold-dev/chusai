open Mtm.Merkletreemap
open Mtm.Sha256
open Tools
module Mtm = MerkleTreeMap (Sha256)

let quickcheck_test_number = 100

let quickcheck_test_to_list_from_list =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:"input = to_list @@ from_list input "
    QCheck.(small_list @@ pair small_string int)
    (fun input_list ->
      Mtm.(
        let input_unique = Tools.unique input_list in
        input_unique |> Mtm.from_list |> to_list = Tools.sorted input_unique))
;;

let quickcheck_test_remove_upsert_remove =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:" to_list . remove k . upsert k v . from_list = to_list . remove k . from_list "
    QCheck.(pair (pair small_string int) (small_list @@ pair small_string int))
    (fun (((k, v), input_list) : ('k * 'v) * ('k * 'v) list) ->
      let left =
        input_list
        |> Mtm.from_list
        |> Mtm.upsert k v
        |> Tools.third
        |> Mtm.remove k
        |> Tools.third
        |> Mtm.to_list
      in
      let right =
        input_list |> Mtm.from_list |> Mtm.remove k |> Tools.third |> Mtm.to_list
      in
      compare left right = 0)
;;

let quickcheck_test_remove =
  let mygen (karb : 'k QCheck.arbitrary) (varb : 'v QCheck.arbitrary)
      : ('k * ('k * 'v) list) QCheck.arbitrary
    =
    let open QCheck in
    triple (pair karb varb) (small_list (pair karb varb)) (small_list (pair karb varb))
    |> map (fun ((k, v), lst1, lst2) ->
           let all_items = List.concat [ lst1; [ k, v ]; lst2 ] in
           let lst = Tools.unique all_items in
           k, lst)
  in
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:
      "sorted . List.remove  random . unique = to_list . Mtm.remove random . from_list"
    QCheck.(mygen small_printable_string int)
    (fun (key_to_remove, input_unique) ->
      let _ = Mtm__Debug.print "key_to_remove" key_to_remove in
      let _ = Mtm__Debug.print "input_unique" input_unique in
      let input_list_without_key = Tools.remove_key key_to_remove input_unique in
      let _ = Mtm__Debug.print "input_list_without_key" input_list_without_key in
      let left_side = Tools.sorted input_list_without_key in
      let right_side =
        Mtm.to_list
        @@ Tools.third
        @@ Mtm.remove key_to_remove
        @@ Mtm.from_list input_unique
      in
      let _ = Mtm__Debug.print "result:" (left_side = right_side) in
      left_side = right_side)
;;

let quickcheck_test_verify_proof_insert =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:"verify insert"
    QCheck.(pair (pair small_string int) (small_list @@ pair small_string int))
    (fun ((k, v), input_list) ->
      let input_list_without_kv = Tools.remove_key k input_list in
      let initial_map = Mtm.from_list input_list_without_kv in
      let op, proof, final_map = Mtm.upsert k v initial_map in
      Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map))
;;

let quickcheck_test_verify_proof_update =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:"verify update"
    QCheck.(pair (pair small_string int) (small_list @@ pair small_string int))
    (fun ((k, v), input_list_with_kv) ->
      let initial_map = Mtm.from_list input_list_with_kv in
      let op, proof, final_map = Mtm.upsert k v initial_map in
      Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map))
;;

let quickcheck_test_verify_proof_remove_failure =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:"verify remove inexistent key"
    QCheck.(Arbitraries.list_with_key_gen small_printable_string int)
    (fun ((key_to_remove, _), input_unique) ->
      let input_list_without_key = Tools.remove_key key_to_remove input_unique in
      let initial_map = Mtm.from_list input_list_without_key in
      let op, proof, final_map = Mtm.remove key_to_remove initial_map in
      Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map))
;;

let quickcheck_test_verify_proof_remove_leaf =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:"verify remove leaf"
    QCheck.(Arbitraries.nonempty_small_list @@ pair small_printable_string int)
    (fun input_list ->
      let input_unique = Tools.unique input_list in
      (*d the last element in the list is guaranteed to be added as a leaf in the tree *)
      let key_to_remove, _ = CCList.last_opt input_unique |> Option.get in
      let initial_map = Mtm.from_list input_unique in
      let op, proof, final_map = Mtm.remove key_to_remove initial_map in
      Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map))
;;

let quickcheck_test_verify_proof_remove_non_leaf =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:"verify remove non-leaf"
    QCheck.(
      pair
        (pair small_printable_string int)
        (Arbitraries.nonempty_small_list @@ pair small_printable_string int))
    (fun ((key_to_remove, v), input_list) ->
      (* If the key to remove is the first in map then we are guaranteed to have a non-leaf node *)
      let input_unique = Tools.unique @@ ((key_to_remove, v) :: input_list) in
      let initial_map = Mtm.from_list input_unique in
      let op, proof, final_map = Mtm.remove key_to_remove initial_map in
      Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map))
;;

let quickcheck_test_verify_proof_remove_non_leaf_2 =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:"verify remove any node (small)"
    QCheck.(
      pair
        (pair small_printable_string int)
        (list_of_size (int_range 1 1).gen @@ pair small_printable_string int))
    (fun ((key_to_remove, v), input_list) ->
      (* If the key to remove is the first in map then we are guaranteed to have a non-leaf node *)
      let input_unique = Tools.unique @@ ((key_to_remove, v) :: input_list) in
      let initial_map = Mtm.from_list input_unique in
      let op, proof, final_map = Mtm.remove key_to_remove initial_map in
      Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map))
;;
