open Mtm.Serializer_algo

module Dump : SERIALIZER = struct
  let serialize a = Batteries.dump a |> Bytes.of_string
end

module Sha3_256 = Mtm.Sha3_256.Make (Dump)
module Mtm = Mtm.Merkle_tree_map.Make (Sha3_256)

let sorted lst = List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) lst
let unique lst = CCList.uniq ~eq:(fun (k1, _) (k2, _) -> k1 = k2) lst

let remove_key key_to_remove lst =
  List.filter (fun pair -> String.compare (fst pair) key_to_remove != 0) lst
;;

let check_lists = Alcotest.(check (list (pair string int))) "same lists"
let check_true = Alcotest.(check bool "assert true" true)
let check_false = Alcotest.(check bool "assert false" false)
let third (_a, _b, c) = c
