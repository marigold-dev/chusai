#import "../src/verifier.mligo" "Verifier"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#include "../../commons/verifier_interface.mligo"

let empty_int_list = ([] : int list)


let compare_string (left  : string) (right : string) : int = 
  if left < right then -1
  else if left > right then 1
  else 0

let hash_string (s : string) : hash_t = Bytes.pack s

let hash_nat (n : nat) : hash_t = Bytes.pack n

let verifier : (string, nat) map_verifier = {
  compare_key = compare_string;
  hash_key = hash_string;
  hash_value = hash_nat
}

let _test_insert_in_empty_map () = 
  let proof = [NewLeaf {vhash = hash_nat 1n; key = "a"}] in
  let op = Upsert {key="a"; value = 1n} in
  let initial_hash = Bytes.pack "todo" in
  let final_hash = Bytes.pack "todo" in
  Unit.assert_ 
    (Verifier.verify_proof verifier op proof initial_hash final_hash) 
    "verification should succeed"
