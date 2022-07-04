(* exec: ligo run test test_pack.mligo

   The generated result is used in Test_pack for test.
   It represents in a pair. The first element is message,
   matching message in Test_pack.
*)

let _test_pack (type a) (msg:string) (data:a) : unit =
  Test.log (msg, Bytes.pack data)

let test_pack_bytes =
  let () = _test_pack "test pack arbitrary bytes" 0x0974657374207061636b in
  let () = _test_pack "test pack bytes 00" 0x00 in
  ()

let test_string =
  let () = _test_pack "test pack arbitrary string" "pack test" in
  let () = _test_pack "test pack empty string" "" in
  ()

let test_int =
  let () = _test_pack "test pack arbitrary postive int" 1 in
  let () = _test_pack "test pack arbitrary negative int" (-1) in
  let () = _test_pack "test pack int 0" 0 in
  ()

let test_list =
  let () = _test_pack "test pack empty list of int" ([] : int list) in
  let () = _test_pack "test pack list of int" [1;2;3;4] in
  let () = _test_pack "test pack empty list of string" ([] : string list) in
  let () = _test_pack "test pack list of pair" [(1, "one"); (2, "two")] in
  ()

let test_pair =
  let () = _test_pack "test pack pair" ("test", 100) in
  ()
