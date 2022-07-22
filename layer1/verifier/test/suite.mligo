#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#include "../../commons/verifier_interface.mligo"
#import "../src/verifier.mligo" "Verifier"

let hash_nat (n : nat) : hash_t = Bytes.pack n
let hash_string (s : string) : hash_t = Bytes.pack s

let _test_key_is_not_found_in_empty_map() = 
  let proof = [ (NotFound : string proof_step)] 
  in
  let root_hash = Bytes.pack "todo" in

  Unit.assert_ 
    (Verifier.verify_proof hash_string proof root_hash) 
    "verification should succeed"

let _test_key_is_found_in_root() = 
  let proof = 
    [ Found 
      { vhash = hash_nat 1n
      ; key = "a"
      ; lhash = Bytes.pack "todo"
      ; rhash = Bytes.pack "todo"
      }
    ] in
  let root_hash = Bytes.pack "todo" in

  Unit.assert_ 
    (Verifier.verify_proof hash_string proof root_hash) 
    "verification should succeed"


let _test_left_right_notfound() = 
  let proof = 
    [ GoLeft 
      { key = "b"
      ; vhash = Bytes.pack "todo"
      ; rhash = Bytes.pack "todo"
      }
    ; GoRight
      { key =  "a"
      ; vhash = Bytes.pack "todo"
      ; lhash = Bytes.pack "todo"
      }
    ; (NotFound : string proof_step) 
    ] 
  in
  let root_hash = Bytes.pack "todo" in

  Unit.assert_ 
    (Verifier.verify_proof hash_string proof root_hash) 
    "verification should succeed"

let _test_left_right_found() = 
  let proof = 
    [ GoLeft 
      { key = "b"
      ; vhash = Bytes.pack "todo"
      ; rhash = Bytes.pack "todo"
      }
    ; GoRight
      { key = "a"
      ; vhash = Bytes.pack "todo"
      ; lhash = Bytes.pack "todo"
      }
    ; Found 
      { vhash = hash_nat 1n
      ; key = "a"
      ; lhash = Bytes.pack "todo"
      ; rhash = Bytes.pack "todo"
      }
    ] in
  let root_hash = Bytes.pack "todo" in

  Unit.assert_ 
    (Verifier.verify_proof hash_string proof root_hash) 
    "verification should succeed"

let suite = Unit.make_suite
  "verifier"
  "Test suite for Verifier"
  [ Unit.make_test "Verify" "key NOT found empty map" _test_key_is_not_found_in_empty_map 
  ; Unit.make_test "Verify" "key found in root" _test_key_is_found_in_root
  ; Unit.make_test "Verify" "GoLeft/GoRight/Found" _test_left_right_found
  ; Unit.make_test "Verify" "GoLeft/GoRight/NotFound" _test_left_right_notfound
  ]