open Chusai_lib

let of_bytes s = (Bytes.of_string (Hex.to_string (`Hex s)))

module Pack = struct
  open Serializable.Pack

  let test_bytes () =
    let pack b = Serializable.Pack.pack (Bytes_t, of_bytes b) in

    Alcotest.(check bytes) "test pack arbitrary bytes"
      (of_bytes "050a0000000a0974657374207061636b")
      (pack "0974657374207061636b");
    Alcotest.(check bytes) "test pack bytes 00"
      (of_bytes "050a0000000100")
      (pack "00")

  let test_string () =
    let pack s = Serializable.Pack.pack (String_t, s) in

    Alcotest.(check bytes) "test pack arbitrary string"
      (of_bytes "0501000000097061636b2074657374")
      (pack "pack test");
    Alcotest.(check bytes) "test pack empty string"
      (of_bytes "050100000000")
      (pack "")

  let test_int () =
    let pack i = Serializable.Pack.pack (Int_t, Z.of_int i) in

    Alcotest.(check bytes) "test pack arbitrary postive int"
      (of_bytes "050001")
      (pack 1);
    Alcotest.(check bytes) "test pack arbitrary negative int"
      (of_bytes "050041")
      (pack (-1));
    Alcotest.(check bytes) "test pack int 0"
      (of_bytes "050000")
      (pack 0)

  let test_list () =
    let pack l = Serializable.Pack.pack (List_t Int_t, List.map Z.of_int l) in

    Alcotest.(check bytes) "test pack empty list of int"
      (of_bytes "050200000000")
      (pack []);
    Alcotest.(check bytes) "test pack list of int"
      (of_bytes "0502000000080001000200030004")
      (pack [1;2;3;4]);

    Alcotest.(check bytes) "test pack empty list of string"
      (of_bytes "050200000000")
      (Serializable.Pack.pack (List_t String_t, []));
    Alcotest.(check bytes) "test pack list of pair"
      (of_bytes "0502000000180707000101000000036f6e6507070002010000000374776f")
      (Serializable.Pack.pack (List_t (Pair_t (Int_t, String_t)), [(Z.one,"one"); (Z.of_int 2, "two")]))

  let test_pair () =
    Alcotest.(check bytes) "test pack pair"
      (of_bytes "05070701000000047465737400a401")
      (Serializable.Pack.pack (Pair_t (String_t, Int_t), ("test", Z.of_int 100)))

  let test_tuple () =
    Alcotest.(check bytes) "test pack tuple"
      (of_bytes "0509070000001501000000047465737400a40101000000047061636b00000000")
      (Serializable.Pack.pack (Tuple_t (String_t, Int_t, String_t), ("test", Z.of_int 100, "pack")))

  let test () =
    let open Alcotest in
    "pack test", [
      test_case "bytes" `Quick test_bytes;
      test_case "string" `Quick test_string;
      test_case "int" `Quick test_int;
      test_case "list" `Quick test_list;
      test_case "pair" `Quick test_pair;
      test_case "tuple" `Quick test_tuple;
    ] ;
end

module Pack_Tezos = struct
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

let () =
  let open Alcotest in
  run "Utils" [
    Pack.test ();
    Pack_Tezos.test ()
  ]
