open Mtm.Merkletreemap
open Mtm.Sha256
open Tools

module Mtm = MerkleTreeMap(Sha256)

let test_empty () =
  check_lists [] Mtm.(to_list empty_mtm)

let test_from_empty_list () = check_lists [] Mtm.(to_list @@ mtm_from_list [])

let test_from_one_elem_list () = 
  check_lists 
    [("a", 1)]
    (Mtm.to_list @@ mtm_from_list @@ [("a", 1)])

let test_from_two_elem_list () = 
  check_lists 
  [("a", 1); ("b", 2)]
  (Mtm.to_list @@ mtm_from_list @@ [("a", 1); ("b", 2)])

let test_from_three_elem_list () = 
  check_lists 
  [("a", 1); ("b", 2); ("c", 3)]
  (Mtm.to_list @@ mtm_from_list @@ [ ("b", 2); ("a", 1);("c", 3)])
    
let test_remove_from_empty () = 
  check_lists  
    [] Mtm.(to_list @@ Tools.third @@ remove "non-existent" empty_mtm)

let test_remove_missing_key () =
  check_lists 
    ( [("b", 0)]
      |> mtm_from_list
      |> Mtm.remove "non-existent" 
      |> Tools.third
      |> Mtm.to_list)
    ( [("b",0)])

let test_remove_leaf () = 
  check_lists  
    [] Mtm.(to_list @@ Tools.third @@ remove "a" @@ mtm_from_list [("a", 1)])

let test_remove_node_right () = 
  let initial_map = mtm_from_list [("a", 1); ("b", 2)] in
  let _op, _proof, final_map = Mtm.remove "a" initial_map in
  let _ = Format.printf "initial_map=%s\n" @@ Mtm.show initial_map in 
  let _ = Format.printf "final_map=%s\n" @@ Mtm.show final_map in 
  check_lists  
    [("b", 2)] (Mtm.to_list final_map)

let test_remove_node_right_right () = 
  check_lists  
    [("b", 2); ("c", 3)] Mtm.(to_list @@ Tools.third @@ remove "a" @@ mtm_from_list [("a", 1); ("b", 2); ("c", 3)])


let test_remove_node_right_left () = 
  let initial_map = mtm_from_list [("a", 1); ("c", 3); ("b", 2)] in
  let _, _, final_map = Mtm.remove "a" initial_map in
  let _ = Format.printf "initial_map=%s\n" @@ Mtm.show initial_map in 
  let _ = Format.printf "final_map=%s\n" @@ Mtm.show final_map in 
  check_lists  
    [("b", 2); ("c", 3)] (Mtm.to_list final_map)

let test_remove_2 () = 
  check_lists  
    [("a", 1); ("c", 3); ("d", 4)] Mtm.(to_list @@ Tools.third @@ remove "b" @@ mtm_from_list [("b", 2); ("d", 4); ("c", 3); ("a", 1)])

let test_remove_node_right_left_left_left () = 
  check_lists  
    [("b", 2); ("c", 3); ("d", 4); ("e", 5)] Mtm.(to_list @@ Tools.third @@ remove "a" @@ mtm_from_list [("a", 1); ("e", 5); ("d", 4); ("c", 3); ("b", 2)])

let test_remove_node_right_left_y () = 
  check_lists  
    [("b", 2); ("c", 3); ("d", 4); ("e", 5)] Mtm.(to_list @@ Tools.third @@ remove "a" @@ mtm_from_list [("a", 1); ("e", 5); ("d", 4); ("b", 2); ("c", 3)])

    
let test_remove_node_left () = 
  check_lists  
    [("a", 1)] Mtm.(to_list @@ Tools.third @@ remove "b" @@ mtm_from_list [("b", 2); ("a", 1)])

let test_remove_node_left_left () = 
  check_lists  
    [("a", 1); ("b", 2)] Mtm.(to_list @@ Tools.third @@ remove "c" @@ mtm_from_list [("c", 3); ("b", 2); ("a", 1)])

let test_remove_y_node () = 
  check_lists  
    [("a", 1); ("c", 3)] Mtm.(to_list @@ Tools.third @@ remove "b" @@ mtm_from_list [("a", 1); ("b", 2); ("c", 3)])
    

let check_pair = Alcotest.(check (option (pair string int)) "same key-value")

let test_verify_proof_insert_in_empty_list () =
  let k,v = "a", 1 in
  let input_list = [] in
  let open Mtm in
  let initial_map = mtm_from_list input_list in
  let _ = Format.printf "initial map=%s\n" @@ Mtm.show initial_map in
  let op, proof, final_map = upsert k v initial_map in
  let _ = Format.printf "final map=%s\n" @@ Mtm.show final_map in
  check_true @@ Tools.verify_proof op proof (root_hash initial_map) (root_hash final_map)
  
let test_verify_proof_insert_right_in_one_elem_list () =
  let k,v = "b", 2 in
  let input_list = [("a", 1)] in
  let open Mtm in
  let initial_map = mtm_from_list input_list in
  let _ = Format.printf "initial map=%s\n" @@ Mtm.show initial_map in
  let op, proof, final_map = upsert k v initial_map in
  let _ = Format.printf "final map=%s\n" @@ Mtm.show final_map in
  check_true @@ Tools.verify_proof op proof (root_hash initial_map) (root_hash final_map)
    
let test_verify_proof_insert_left_in_one_elem_list () =
  let k,v = "a", 1 in
  let input_list = [("b", 2)] in
  let open Mtm in
  let initial_map = mtm_from_list input_list in
  let _ = Format.printf "initial map=%s\n" @@ Mtm.show initial_map in
  let op, proof, final_map = upsert k v initial_map in
  let _ = Format.printf "final map=%s\n" @@ Mtm.show final_map in
  check_true @@ Tools.verify_proof op proof (root_hash initial_map) (root_hash final_map)
  
  
let test_verify_proof_update_one_elem_list () =
  let k,v = "a", 2 in
  let input_list = [("a", 1)] in
  let open Mtm in
  let initial_map = mtm_from_list input_list in
  let _ = Mtm__Debug.print "initial map" initial_map in
  let op, proof, final_map = upsert k v initial_map in
  let _ = Mtm__Debug.print "final map" final_map in
  let _ = Mtm__Debug.print "proof" proof in
  check_true @@ Tools.verify_proof op proof (root_hash initial_map) (root_hash final_map)

  let test_verify_invalid_proof_insert () =
    let open Mtm in
    let map_a = mtm_from_list [("a", 0)] in
    let map_c = mtm_from_list [("c", 0)] in
    let op_ab, proof_ab, map_ab = upsert "b" 1 map_a in
    let op_bc, proof_bc, map_bc = upsert "b" 1 map_c in

    let _ = check_true @@ Tools.verify_proof op_ab proof_ab (root_hash map_a) (root_hash map_ab) in
    let _ = check_true @@ Tools.verify_proof op_bc proof_bc (root_hash map_c) (root_hash map_bc) in
    let _ = check_false @@ Tools.verify_proof op_bc proof_bc (root_hash map_c) (root_hash map_bc) in
    ()
let test_verify_remove_from_empty_map () =
  let open Mtm in
  let op, proof, final_map = remove "a" empty_mtm in

  let _ = check_true @@ Tools.verify_proof op proof (root_hash empty_mtm) (root_hash final_map) in
  ()

let test_verify_interchange_remove_proofs () =
  let open Mtm in
  let map_a = mtm_from_list [("a", 1)] in
  let map_c = mtm_from_list [("c", 1)] in
  let op_a, proof_a, final_map_a = remove "b" map_a in
  let op_c, proof_c, final_map_c = remove "b" map_c in

  let _ = check_true @@ Tools.verify_proof op_a  proof_a (root_hash map_a) (root_hash final_map_a) in
  let _ = check_false @@ Tools.verify_proof op_c proof_c (root_hash map_a) (root_hash final_map_a) in
  let _ = check_true @@ Tools.verify_proof op_c proof_c (root_hash map_c) (root_hash final_map_c) in
  let _ = check_false @@ Tools.verify_proof op_a proof_a (root_hash map_a) (root_hash final_map_c) in
  ()
let test_verify_remove_existing_key_right () =
  let open Mtm in
  let initial_map = mtm_from_list [("a", 1); ("b",2)] in
  let _ = Mtm__Debug.print "initial map" initial_map in
  let op, proof, final_map = remove "b" initial_map in
  let _ = Mtm__Debug.print "final map" final_map in

  check_true @@ Tools.verify_proof op proof (root_hash initial_map) (root_hash final_map)

let test_verify_remove_existing_key_left () =
  let open Mtm in
  let initial_map = mtm_from_list [("b",2); ("a", 1)] in
  let _ = Format.printf "initial_map:%s\n" @@ show initial_map in
  let op, proof, final_map = remove "a" initial_map in
  let _ = Format.printf "final_map:%s\n" @@ show final_map in

  check_true @@ Tools.verify_proof op proof (root_hash initial_map) (root_hash final_map)


let test_verify_proof_remove_non_leaf_2 () =
  let (key_to_remove, v) =  ("a", 1) in
  let input_list = (key_to_remove, v) :: [("b", 2)] in
  let open Mtm in
  let initial_map = mtm_from_list input_list in
  let _ = Format.printf "initial_map:%s\n" @@ show initial_map in
  let op, proof, final_map = Mtm.remove key_to_remove initial_map in
  let _ = Format.printf "final_map:%s\n" @@ show final_map in
  check_true @@ Tools.verify_proof op proof (root_hash initial_map) (root_hash final_map)
    

let test_quickcheck_failure_1 () =
  let key_to_remove = "c" in
  let v=3 in
  let input_list=[
      ("a",1);
      ("b",2);
      ("d",4);
      ] in
  let open Mtm in
  let input_unique = (key_to_remove, v) :: input_list in
  let initial_map = Tools.mtm_from_list input_unique in
  let _ = Format.printf "initial_map:%s\n" @@ show initial_map in
  let op, proof, final_map = Mtm.remove key_to_remove initial_map in
  let _ = Format.printf "final_map:%s\n" @@ show final_map in
  check_true @@ Tools.verify_proof op proof (root_hash initial_map) (root_hash final_map)


let test_quickcheck_failure_2 () =
  let key_to_remove = "c" in
  let v=3 in
  let input_list=[
      ("d",4);
      ("b",2);
      ("a",1);
    ] in
  let open Mtm in
  let input_unique = (key_to_remove, v) :: input_list in
  let initial_map = Tools.mtm_from_list input_unique in
  let _ = Format.printf "initial_map:%s\n" @@ show initial_map in
  let op, proof, final_map = Mtm.remove key_to_remove initial_map in
  let _ = Format.printf "final_map:%s\n" @@ show final_map in
  check_true @@ Tools.verify_proof op proof (root_hash initial_map) (root_hash final_map)
