open Tools

let test_empty () = check_lists [] (Mtm.to_list Mtm.empty)
let test_from_empty_list () = check_lists [] (Mtm.to_list @@ Mtm.from_list [])

let test_from_one_elem_list () =
  check_lists [ "a", 1 ] (Mtm.to_list @@ Mtm.from_list @@ [ "a", 1 ])
;;

let test_from_two_elem_list () =
  check_lists [ "a", 1; "b", 2 ] (Mtm.to_list @@ Mtm.from_list @@ [ "a", 1; "b", 2 ])
;;

let test_from_three_elem_list () =
  check_lists
    [ "a", 1; "b", 2; "c", 3 ]
    (Mtm.to_list @@ Mtm.from_list @@ [ "b", 2; "a", 1; "c", 3 ])
;;

let test_remove_from_empty () =
  check_lists
    []
    Mtm.(
      to_list
      @@ Tools.get_tree
      @@ Mtm.execute (Mtm.Remove { key = "non-existent" }) Mtm.empty)
;;

let test_remove_missing_key () =
  check_lists
    ([ "b", 0 ]
    |> Mtm.from_list
    |> Mtm.execute (Mtm.Remove { key = "non-existent" })
    |> Tools.get_tree
    |> Mtm.to_list)
    [ "b", 0 ]
;;

let test_remove_leaf () =
  check_lists
    []
    (Mtm.to_list
    @@ Tools.get_tree
    @@ Mtm.execute (Mtm.Remove { key = "a" })
    @@ Mtm.from_list [ "a", 1 ])
;;

let test_remove_node_right () =
  let initial_map = Mtm.from_list [ "a", 1; "b", 2 ] in
  let _op, _proof, final_map = Mtm.execute (Mtm.Remove { key = "a" }) initial_map in
  check_lists [ "b", 2 ] (Mtm.to_list final_map)
;;

let test_remove_node_right_right () =
  check_lists
    [ "b", 2; "c", 3 ]
    (Mtm.to_list
    @@ Tools.get_tree
    @@ Mtm.execute (Mtm.Remove { key = "a" })
    @@ Mtm.from_list [ "a", 1; "b", 2; "c", 3 ])
;;

let test_remove_node_right_left () =
  let initial_map = Mtm.from_list [ "a", 1; "c", 3; "b", 2 ] in
  let _, _, final_map = Mtm.execute (Mtm.Remove { key = "a" }) initial_map in
  check_lists [ "b", 2; "c", 3 ] (Mtm.to_list final_map)
;;

let test_remove_2 () =
  check_lists
    [ "a", 1; "c", 3; "d", 4 ]
    Mtm.(
      to_list
      @@ Tools.get_tree
      @@ Mtm.execute (Mtm.Remove { key = "non_existent" })
      @@ Mtm.from_list [ "b", 2; "d", 4; "c", 3; "a", 1 ])
;;

let test_remove_node_right_left_left_left () =
  check_lists
    [ "b", 2; "c", 3; "d", 4; "e", 5 ]
    Mtm.(
      to_list
      @@ Tools.get_tree
      @@ Mtm.execute (Mtm.Remove { key = "a" })
      @@ Mtm.from_list [ "a", 1; "e", 5; "d", 4; "c", 3; "b", 2 ])
;;

let test_remove_node_right_left_y () =
  check_lists
    [ "b", 2; "c", 3; "d", 4; "e", 5 ]
    Mtm.(
      to_list
      @@ Tools.get_tree
      @@ Mtm.execute (Mtm.Remove { key = "a" })
      @@ Mtm.from_list [ "a", 1; "e", 5; "d", 4; "b", 2; "c", 3 ])
;;

let test_remove_node_left () =
  check_lists
    [ "a", 1 ]
    Mtm.(
      to_list
      @@ Tools.get_tree
      @@ Mtm.execute (Mtm.Remove { key = "b" })
      @@ Mtm.from_list [ "b", 2; "a", 1 ])
;;

let test_remove_node_left_left () =
  check_lists
    [ "a", 1; "b", 2 ]
    Mtm.(
      to_list
      @@ Tools.get_tree
      @@ Mtm.execute (Mtm.Remove { key = "c" })
      @@ Mtm.from_list [ "c", 3; "b", 2; "a", 1 ])
;;

let test_remove_y_node () =
  check_lists
    [ "a", 1; "c", 3 ]
    Mtm.(
      to_list
      @@ Tools.get_tree
      @@ Mtm.execute (Mtm.Remove { key = "b" })
      @@ Mtm.from_list [ "a", 1; "b", 2; "c", 3 ])
;;

let check_pair = Alcotest.(check (option (pair string int)) "same key-value")

let test_verify_proof_insert_in_empty_list () =
  let key, value = "a", 1 in
  let input_list = [] in
  let initial_map = Mtm.from_list input_list in
  let op = Mtm.Upsert { key; value } in
  let None, proof, final_map = Mtm.execute op initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_proof_insert_right_in_one_elem_list () =
  let input_list = [ "a", 1 ] in
  let initial_map = Mtm.from_list input_list in
  let op = Mtm.Upsert { key = "b"; value = 2 } in
  let None, proof, final_map = Mtm.execute op initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_proof_insert_left_in_one_elem_list () =
  let input_list = [ "b", 2 ] in
  let initial_map = Mtm.from_list input_list in
  let op = Mtm.Upsert { key = "a"; value = 2 } in
  let None, proof, final_map = Mtm.execute op initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_proof_update_one_elem_list () =
  let input_list = [ "a", 1 ] in
  let initial_map = Mtm.from_list input_list in
  let op = Mtm.Upsert { key = "a"; value = 2 } in
  let Some _, proof, final_map = Mtm.execute op initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_remove_from_empty_map () =
  let op = Mtm.Remove { key = "a" } in
  let None, proof, final_map = Mtm.execute op Mtm.empty in
  let _ =
    check_true
    @@ Mtm.verify_proof op proof (Mtm.root_hash Mtm.empty) (Mtm.root_hash final_map)
  in
  ()
;;

let test_verify_remove_existing_key_right () =
  let initial_map = Mtm.from_list [ "a", 1; "b", 2 ] in
  let op = Mtm.Remove { key = "b" } in
  let Some _, proof, final_map = Mtm.execute op initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_remove_existing_key_left () =
  let initial_map = Mtm.from_list [ "b", 2; "a", 1 ] in
  let op = Mtm.Remove { key = "a" } in
  let Some _, proof, final_map = Mtm.execute op initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_proof_remove_non_leaf_2 () =
  let input_list = [ "a", 1; "b", 2 ] in
  let initial_map = Mtm.from_list input_list in
  let op = Mtm.Remove { key = "a" } in
  let Some _, proof, final_map = Mtm.execute op initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_proof_for_chain_replacements () =
  let input_list = [ "e", 1; "f", 2; "c", 3; "d", 4; "a", 5; "b", 6 ] in
  let initial_map = Mtm.from_list input_list in
  let op = Mtm.Remove { key = "e" } in
  let Some _, proof, final_map = Mtm.execute op initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_proof_lookup_not_found () =
  let input_list_with_kv = [] in
  let map = Mtm.from_list input_list_with_kv in
  let op = Mtm.Lookup { key = "a" } in
  let None, proof, _ = Mtm.execute op map in
  let result = Mtm.verify_proof op proof (Mtm.root_hash map) (Mtm.root_hash map) in
  check_true result
;;

let test_verify_proof_lookup_found () =
  let input_list_with_kv = [ "a", "0" ] in
  let map = Mtm.from_list input_list_with_kv in
  let op = Mtm.Lookup { key = "a" } in
  let Some _, proof, _ = Mtm.execute op map in
  let result = Mtm.verify_proof op proof (Mtm.root_hash map) (Mtm.root_hash map) in
  check_true result
;;

let test_remove_left_right_right () =
  let input_list = [ "c", 3; "a", 1; "b", 2; "d", 4 ] in
  let initial_map = Mtm.from_list input_list in
  let op = Mtm.Remove { key = "c" } in
  let Some _, proof, final_map = Mtm.execute op initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_remove_right_left_left () =
  let input_list = [ "c", 3; "d", 4; "b", 2; "a", 1 ] in
  let initial_map = Mtm.from_list input_list in
  let op = Mtm.Remove { key = "c" } in
  let Some _, proof, final_map = Mtm.execute op initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;
