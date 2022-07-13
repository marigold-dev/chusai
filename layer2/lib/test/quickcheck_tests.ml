open Tools

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
    ~name:
      " to_list . remove k . Mtm.Upsert k v . from_list = to_list . remove k . from_list "
    QCheck.(pair (pair small_string int) (small_list @@ pair small_string int))
    (fun (((key, value), input_list) : ('k * 'v) * ('k * 'v) list) ->
      let left =
        input_list
        |> Mtm.from_list
        |> Mtm.execute (Mtm.Upsert { key; value })
        |> Tools.get_tree
        |> Mtm.execute (Mtm.Remove { key })
        |> Tools.get_tree
        |> Mtm.to_list
      in
      let right =
        input_list
        |> Mtm.from_list
        |> Mtm.execute (Mtm.Remove { key })
        |> Tools.get_tree
        |> Mtm.to_list
      in
      compare left right = 0)
;;

let quickcheck_test_upsert_lookup =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:" Some v = lookup k . Mtm.Upsert k v . from_list "
    QCheck.(
      pair
        (pair small_string int)
        (Arbitraries.nonempty_small_list @@ pair small_string int))
    (fun (((key, value), input_list) : ('k * 'v) * ('k * 'v) list) ->
      let result =
        input_list
        |> Mtm.from_list
        |> Mtm.execute (Mtm.Upsert { key; value })
        |> Tools.get_tree
        |> Mtm.execute (Mtm.Lookup { key })
        |> Tools.get_result
      in
      Some value = result)
;;

let quickcheck_test_upsert_remove_lookup =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:"None = lookup k . remove k . Mtm.Upsert k v . from_list "
    QCheck.(
      pair
        (pair small_string int)
        (Arbitraries.nonempty_small_list @@ pair small_string int))
    (fun (((key, value), input_list) : ('k * 'v) * ('k * 'v) list) ->
      let result =
        input_list
        |> Mtm.from_list
        |> Mtm.execute (Mtm.Upsert { key; value })
        |> Tools.get_tree
        |> Mtm.execute (Mtm.Remove { key })
        |> Tools.get_tree
        |> Mtm.execute (Mtm.Lookup { key })
        |> Tools.get_result
      in
      Option.is_none result)
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
      let input_list_without_key = Tools.remove_key key_to_remove input_unique in
      let left_side = Tools.sorted input_list_without_key in
      let right_side =
        Mtm.to_list
        @@ Tools.get_tree
        @@ Mtm.execute (Mtm.Remove { key = key_to_remove })
        @@ Mtm.from_list input_unique
      in
      left_side = right_side)
;;

let quickcheck_test_verify_proof_insert =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:"verify insert"
    QCheck.(pair (pair small_string int) (small_list @@ pair small_string int))
    (fun ((key, value), input_list) ->
      let input_list_without_kv = Tools.remove_key key input_list in
      let initial_map = Mtm.from_list input_list_without_kv in
      let op = Mtm.Upsert { key; value } in
      let _, proof, final_map = Mtm.execute op initial_map in
      Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map))
;;

let quickcheck_test_verify_proof_update =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:"verify update"
    QCheck.(pair (pair small_string int) (small_list @@ pair small_string int))
    (fun ((key, value), input_list_with_kv) ->
      let initial_map = Mtm.from_list input_list_with_kv in
      let op = Mtm.Upsert { key; value } in
      let _, proof, final_map = Mtm.execute op initial_map in
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
      let op = Mtm.Remove { key = key_to_remove } in
      let None, proof, final_map = Mtm.execute op initial_map in
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
      let op = Mtm.Remove { key = key_to_remove } in
      let Some _, proof, final_map = Mtm.execute op initial_map in
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
      let op = Mtm.Remove { key = key_to_remove } in
      let _, proof, final_map = Mtm.execute op initial_map in
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
      let op = Mtm.Remove { key = key_to_remove } in
      let _, proof, final_map = Mtm.execute op initial_map in
      Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map))
;;

let quickcheck_test_verify_proof_lookup_found =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:"verify lookup key found"
    QCheck.(
      pair
        (pair small_printable_string int)
        (list_of_size (int_range 1 1).gen @@ pair small_printable_string int))
    (fun ((key, v), input_list) ->
      let input_unique = Tools.unique @@ ((key, v) :: input_list) in
      let m = Mtm.from_list input_unique in
      let op = Mtm.Lookup { key } in
      let Some _, proof, _ = Mtm.execute op m in
      let h = Mtm.root_hash m in
      Mtm.verify_proof op proof h h)
;;

let quickcheck_test_verify_proof_lookup_not_found =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:"verify lookup key NOT found"
    QCheck.(
      pair
        (pair small_printable_string int)
        (list_of_size (int_range 1 1).gen @@ pair small_printable_string int))
    (fun ((key, v), input_list) ->
      let input_unique = Tools.unique @@ Tools.remove_key key input_list in
      let m = Mtm.from_list input_unique in
      let op = Mtm.Lookup { key } in
      let None, proof, _ = Mtm.execute op m in
      let h = Mtm.root_hash m in
      Mtm.verify_proof op proof h h)
;;
