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

let check_pair = Alcotest.(check (option (pair string int)) "same key-value")

let test_verify_proof_lookup_not_found () =
  let input_list_with_kv = [] in
  let map = Mtm.from_list input_list_with_kv in
  let result, proof = Mtm.lookup "a" map in
  let _ = check_true @@ Option.is_none result in

  let verification_result = Mtm.verify_proof proof (Mtm.root_hash map) in
  check_true verification_result
;;

let test_verify_proof_lookup_found () =
  let input_list_with_kv = [ "a", 0 ] in
  let map = Mtm.from_list input_list_with_kv in
  
  let result, proof = Mtm.lookup "a" map in
  let _ = check_int_opts (Some 0) result in

  let verification_result = Mtm.verify_proof proof (Mtm.root_hash map) in
  check_true verification_result
;;

