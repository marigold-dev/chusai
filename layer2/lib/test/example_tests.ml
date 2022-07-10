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
  check_lists [] Mtm.(to_list @@ Tools.third @@ Mtm.remove "non-existent" Mtm.empty)
;;

let test_remove_missing_key () =
  check_lists
    ([ "b", 0 ]
    |> Mtm.from_list
    |> Mtm.remove "non-existent"
    |> Tools.third
    |> Mtm.to_list)
    [ "b", 0 ]
;;

let test_remove_leaf () =
  check_lists [] (Mtm.to_list @@ Tools.third @@ Mtm.remove "a" @@ Mtm.from_list [ "a", 1 ])
;;

let test_remove_node_right () =
  let initial_map = Mtm.from_list [ "a", 1; "b", 2 ] in
  let _op, _proof, final_map = Mtm.remove "a" initial_map in
  check_lists [ "b", 2 ] (Mtm.to_list final_map)
;;

let test_remove_node_right_right () =
  check_lists
    [ "b", 2; "c", 3 ]
    (Mtm.to_list
    @@ Tools.third
    @@ Mtm.remove "a"
    @@ Mtm.from_list [ "a", 1; "b", 2; "c", 3 ])
;;

let test_remove_node_right_left () =
  let initial_map = Mtm.from_list [ "a", 1; "c", 3; "b", 2 ] in
  let _, _, final_map = Mtm.remove "a" initial_map in
  check_lists [ "b", 2; "c", 3 ] (Mtm.to_list final_map)
;;

let test_remove_2 () =
  check_lists
    [ "a", 1; "c", 3; "d", 4 ]
    Mtm.(
      to_list
      @@ Tools.third
      @@ Mtm.remove "b"
      @@ Mtm.from_list [ "b", 2; "d", 4; "c", 3; "a", 1 ])
;;

let test_remove_node_right_left_left_left () =
  check_lists
    [ "b", 2; "c", 3; "d", 4; "e", 5 ]
    Mtm.(
      to_list
      @@ Tools.third
      @@ Mtm.remove "a"
      @@ Mtm.from_list [ "a", 1; "e", 5; "d", 4; "c", 3; "b", 2 ])
;;

let test_remove_node_right_left_y () =
  check_lists
    [ "b", 2; "c", 3; "d", 4; "e", 5 ]
    Mtm.(
      to_list
      @@ Tools.third
      @@ Mtm.remove "a"
      @@ Mtm.from_list [ "a", 1; "e", 5; "d", 4; "b", 2; "c", 3 ])
;;

let test_remove_node_left () =
  check_lists
    [ "a", 1 ]
    Mtm.(to_list @@ Tools.third @@ Mtm.remove "b" @@ Mtm.from_list [ "b", 2; "a", 1 ])
;;

let test_remove_node_left_left () =
  check_lists
    [ "a", 1; "b", 2 ]
    Mtm.(
      to_list @@ Tools.third @@ Mtm.remove "c" @@ Mtm.from_list [ "c", 3; "b", 2; "a", 1 ])
;;

let test_remove_y_node () =
  check_lists
    [ "a", 1; "c", 3 ]
    Mtm.(
      to_list @@ Tools.third @@ Mtm.remove "b" @@ Mtm.from_list [ "a", 1; "b", 2; "c", 3 ])
;;

let check_pair = Alcotest.(check (option (pair string int)) "same key-value")

let test_verify_proof_insert_in_empty_list () =
  let k, v = "a", 1 in
  let input_list = [] in
  let initial_map = Mtm.from_list input_list in
  let op, proof, final_map = Mtm.upsert k v initial_map in
  let _ =
    Format.printf "op:%s\n" @@ Mtm.show_op Format.pp_print_string Format.pp_print_int op
  in
  let _ = Format.printf "proof:%s\n" @@ Mtm.show_proof Format.pp_print_string proof in
  let _ =
    Format.printf "final_map:%s\n"
    @@ Mtm.show Format.pp_print_string Format.pp_print_int final_map
  in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_proof_insert_right_in_one_elem_list () =
  let k, v = "b", 2 in
  let input_list = [ "a", 1 ] in
  let initial_map = Mtm.from_list input_list in
  let op, proof, final_map = Mtm.upsert k v initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_proof_insert_left_in_one_elem_list () =
  let k, v = "a", 1 in
  let input_list = [ "b", 2 ] in
  let initial_map = Mtm.from_list input_list in
  let _ =
    Format.printf "initial_map:%s\n"
    @@ Mtm.show Format.pp_print_string Format.pp_print_int initial_map
  in
  let op, proof, final_map = Mtm.upsert k v initial_map in
  let _ =
    Format.printf "op:%s\n" @@ Mtm.show_op Format.pp_print_string Format.pp_print_int op
  in
  let _ = Format.printf "proof:%s\n" @@ Mtm.show_proof Format.pp_print_string proof in
  let _ =
    Format.printf "final_map:%s\n"
    @@ Mtm.show Format.pp_print_string Format.pp_print_int final_map
  in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_proof_update_one_elem_list () =
  let k, v = "a", 2 in
  let input_list = [ "a", 1 ] in
  let initial_map = Mtm.from_list input_list in
  let op, proof, final_map = Mtm.upsert k v initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_invalid_proof_insert () =
  let map_a = Mtm.from_list [ "a", 0 ] in
  let map_c = Mtm.from_list [ "c", 0 ] in
  let op_ab, proof_ab, map_ab = Mtm.upsert "b" 1 map_a in
  let op_bc, proof_bc, map_bc = Mtm.upsert "b" 1 map_c in
  let _ =
    check_true
    @@ Mtm.verify_proof op_ab proof_ab (Mtm.root_hash map_a) (Mtm.root_hash map_ab)
  in
  let _ =
    check_true
    @@ Mtm.verify_proof op_bc proof_bc (Mtm.root_hash map_c) (Mtm.root_hash map_bc)
  in
  let _ =
    check_false
    @@ Mtm.verify_proof op_bc proof_bc (Mtm.root_hash map_c) (Mtm.root_hash map_bc)
  in
  ()
;;

let test_verify_remove_from_empty_map () =
  let op, proof, final_map = Mtm.remove "a" Mtm.empty in
  let _ =
    check_true
    @@ Mtm.verify_proof op proof (Mtm.root_hash Mtm.empty) (Mtm.root_hash final_map)
  in
  ()
;;

let test_verify_interchange_remove_proofs () =
  let map_a = Mtm.from_list [ "a", 1 ] in
  let map_c = Mtm.from_list [ "c", 1 ] in
  let op_a, proof_a, final_map_a = Mtm.remove "b" map_a in
  let op_c, proof_c, final_map_c = Mtm.remove "b" map_c in
  let _ =
    check_true
    @@ Mtm.verify_proof op_a proof_a (Mtm.root_hash map_a) (Mtm.root_hash final_map_a)
  in
  let _ =
    check_false
    @@ Mtm.verify_proof op_c proof_c (Mtm.root_hash map_a) (Mtm.root_hash final_map_a)
  in
  let _ =
    check_true
    @@ Mtm.verify_proof op_c proof_c (Mtm.root_hash map_c) (Mtm.root_hash final_map_c)
  in
  let _ =
    check_false
    @@ Mtm.verify_proof op_a proof_a (Mtm.root_hash map_a) (Mtm.root_hash final_map_c)
  in
  ()
;;

let test_verify_remove_existing_key_right () =
  let initial_map = Mtm.from_list [ "a", 1; "b", 2 ] in
  let _ = Mtm__Debug.print "initial map" initial_map in
  let op, proof, final_map = Mtm.remove "b" initial_map in
  let _ = Mtm__Debug.print "final map" final_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_remove_existing_key_left () =
  let initial_map = Mtm.from_list [ "b", 2; "a", 1 ] in
  let op, proof, final_map = Mtm.remove "a" initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_proof_remove_non_leaf_2 () =
  let key_to_remove = "a" in
  let input_list = [ "a", 1; "b", 2 ] in
  let initial_map = Mtm.from_list input_list in
  let op, proof, final_map = Mtm.remove key_to_remove initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_proof_for_chain_replacements () =
  let key_to_remove = "e" in
  let input_list = [ "e", 1; "f", 2; "c", 3; "d", 4; "a", 5; "b", 6 ] in
  let initial_map = Mtm.from_list input_list in
  let op, proof, final_map = Mtm.remove key_to_remove initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_verify_proof_lookup_not_found () =
  let k, v = "a", 0 in
  let input_list_with_kv = [] in
  let map = Mtm.from_list input_list_with_kv in
  let op, proof, None = Mtm.lookup k map in
  let result = Mtm.verify_proof op proof (Mtm.root_hash map) (Mtm.root_hash map) in
  check_true result
;;

let test_verify_proof_lookup_found () =
  let k, v = "a", 0 in
  let input_list_with_kv = [ "a", "0" ] in
  let map = Mtm.from_list input_list_with_kv in
  let op, proof, Some v = Mtm.lookup k map in
  let result = Mtm.verify_proof op proof (Mtm.root_hash map) (Mtm.root_hash map) in
  check_true result
;;

let test_quickcheck_regression_1 () =
  let key_to_remove = "c" in
  let v = 3 in
  let input_list = [ "a", 1; "b", 2; "d", 4 ] in
  let input_unique = (key_to_remove, v) :: input_list in
  let initial_map = Mtm.from_list input_unique in
  let _ =
    Format.printf "initial_map:%s\n"
    @@ Mtm.show Format.pp_print_string Format.pp_print_int initial_map
  in
  let op, proof, final_map = Mtm.remove key_to_remove initial_map in
  let _ =
    Format.printf "op:%s\n" @@ Mtm.show_op Format.pp_print_string Format.pp_print_int op
  in
  let _ = Format.printf "proof:%s\n" @@ Mtm.show_proof Format.pp_print_string proof in
  let _ =
    Format.printf "final_map:%s\n"
    @@ Mtm.show Format.pp_print_string Format.pp_print_int final_map
  in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_quickcheck_regression_2 () =
  let key_to_remove = "c" in
  let v = 3 in
  let input_list = [ "d", 4; "b", 2; "a", 1 ] in
  let input_unique = (key_to_remove, v) :: input_list in
  let initial_map = Mtm.from_list input_unique in
  let _ =
    Format.printf "initial_map:%s\n"
    @@ Mtm.show Format.pp_print_string Format.pp_print_int initial_map
  in
  let op, proof, final_map = Mtm.remove key_to_remove initial_map in
  let _ =
    Format.printf "op:%s\n" @@ Mtm.show_op Format.pp_print_string Format.pp_print_int op
  in
  let _ = Format.printf "proof:%s\n" @@ Mtm.show_proof Format.pp_print_string proof in
  let _ =
    Format.printf "final_map:%s\n"
    @@ Mtm.show Format.pp_print_string Format.pp_print_int final_map
  in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_quickcheck_regression_3 () =
  let k = "" in
  let v = 1 in
  let input_list = [ "", 0; "", 0; "", -1 ] in
  let input_list_without_kv = Tools.remove_key k input_list in
  let initial_map = Mtm.from_list input_list_without_kv in
  let op, proof, final_map = Mtm.upsert k v initial_map in
  check_true
  @@ Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
;;

let test_quickcheck_regression_4 () =
  let k, v = "", 0 in
  let input_list_with_kv = [ "", 0; "", -1 ] in
  let initial_map = Mtm.from_list input_list_with_kv in
  let op, proof, final_map = Mtm.upsert k v initial_map in
  let result =
    Mtm.verify_proof op proof (Mtm.root_hash initial_map) (Mtm.root_hash final_map)
  in
  check_true result
;;
