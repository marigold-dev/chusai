open Chusai_serialization

module Tezos_micheline_repr = struct
  let test_bytes () =
    let open Serializable_tezos.Pack in
    let test_tezos_bytes = (Bytes_t, Bytes.of_string "test_pack") in
    let open Serializable.Pack in
    let test_bytes = (Bytes_t, Bytes.of_string "test_pack") in

    Alcotest.(check bytes) "test pack bytes"
      (Serializable_tezos.Pack.pack test_tezos_bytes)
      (Serializable.Pack.pack test_bytes)

  let test_string () =
    let open Serializable_tezos.Pack in
    let test_tezos_string = (String_t, "test pack") in
    let open Serializable.Pack in
    let test_string = (String_t, "test pack") in

    Alcotest.(check bytes) "test pack string"
      (Serializable_tezos.Pack.pack test_tezos_string)
      (Serializable.Pack.pack test_string)

  let test_int () =
    let open Serializable_tezos.Pack in
    let test_tezos_int = (Int_t, Z.one) in
    let open Serializable.Pack in
    let test_int = (Int_t, Z.one) in

    Alcotest.(check bytes) "test pack int"
      (Serializable_tezos.Pack.pack test_tezos_int)
      (Serializable.Pack.pack test_int)

  let test_empty_list () =
    let open Serializable_tezos.Pack in
    let test_tezos_list = (List_t Int_t, []) in
    let open Serializable.Pack in
    let test_list = (List_t Int_t, []) in

    Alcotest.(check bytes) "test pack empty list"
      (Serializable_tezos.Pack.pack test_tezos_list)
      (Serializable.Pack.pack test_list)

  let test_list () =
    let open Serializable_tezos.Pack in
    let test_tezos_list = (List_t Int_t, List.map Z.of_int [1;2;3;4]) in
    let open Serializable.Pack in
    let test_list = (List_t Int_t, List.map Z.of_int [1;2;3;4]) in

    Alcotest.(check bytes) "test pack list"
      (Serializable_tezos.Pack.pack test_tezos_list)
      (Serializable.Pack.pack test_list)

  let test_pair () =
    let open Serializable_tezos.Pack in
    let test_tezos_pair = (Pair_t (Int_t, String_t)), (Z.one, "test") in
    let open Serializable.Pack in
    let test_pair =  (Pair_t (Int_t, String_t)), (Z.one, "test") in

    Alcotest.(check bytes) "test pack pair"
      (Serializable_tezos.Pack.pack test_tezos_pair)
      (Serializable.Pack.pack test_pair)

  let test_tuple () =
    let open Serializable_tezos.Pack in
    let test_tezos_tuple = (Tuple_t (Int_t, String_t, String_t)), (Z.one, "test", "pair") in
    let open Serializable.Pack in
    let test_tuple =  (Tuple_t (Int_t, String_t, String_t)), (Z.one, "test", "pair") in

    Alcotest.(check bytes) "test pack pair"
      (Serializable_tezos.Pack.pack test_tezos_tuple)
      (Serializable.Pack.pack test_tuple)

  let test () =
    let open Alcotest in
    "pack test by comparing to tezos_micheline", [
      test_case "bytes" `Quick test_bytes;
      test_case "string" `Quick test_string;
      test_case "int" `Quick test_int;
      test_case "empty list" `Quick test_empty_list;
      test_case "list" `Quick test_list;
      test_case "pair" `Quick test_pair;
      test_case "tuple" `Quick test_tuple;
    ];
end
