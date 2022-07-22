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
        |> Mtm.upsert key value
        |> Tools.get_tree
        |> Mtm.lookup key
        |> Tools.get_result
      in
      Some value = result)
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
      let result, proof = Mtm.lookup key m in
      let h = Mtm.root_hash m in
      Option.is_some result && Mtm.verify_proof proof h)
;;

let quickcheck_test_verify_proof_lookup_not_found =
  QCheck.Test.make
    ~count:quickcheck_test_number
    ~name:"verify lookup key NOT found"
    QCheck.(
      pair
        small_printable_string
        (list_of_size (int_range 1 1).gen @@ pair small_printable_string int))
    (fun (key, input_list) ->
      let input_unique = Tools.unique @@ Tools.remove_key key input_list in
      let m = Mtm.from_list input_unique in
      let result, proof = Mtm.lookup key m in
      let h = Mtm.root_hash m in
      Option.is_none result && Mtm.verify_proof proof h)
;;
