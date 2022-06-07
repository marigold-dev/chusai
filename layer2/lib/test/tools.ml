open Mtm.Merkletreemap
open Mtm.Sha256

module Mtm = MerkleTreeMap(Sha256)
let sorted lst = List.sort (fun (k1, _) (k2, _)  -> String.compare k1 k2 ) lst
let unique lst = CCList.uniq ~eq:(fun (k1, _) (k2, _) -> k1 = k2) lst

let check_lists =  Alcotest.(check (list (pair string int))) "same lists"
let check_true = Alcotest.(check bool "assert true" true)
let check_false = Alcotest.(check bool "assert false" false)

let mtm_from_list = Mtm.from_list Format.pp_print_string Format.pp_print_int

let empty_mtm = Mtm.empty Format.pp_print_string Format.pp_print_int