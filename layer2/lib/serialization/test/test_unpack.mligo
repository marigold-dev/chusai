(* exec: ligo run test test_unpack.mligo

   The generated result is used in Test_pack for test.
   It represents in a pair. The first element is message,
   matching message in Test_pack.
*)

let _test_unpack_bytes (msg:string) (b:bytes) : unit =
  Test.log (msg, (Bytes.unpack b : bytes option))

let _test_unpack_string (msg:string) (b:bytes) : unit =
  Test.log (msg, (Bytes.unpack b : string option))

let _test_unpack_int (msg:string) (b:bytes) : unit =
  Test.log (msg, (Bytes.unpack b : int option))

let _test_unpack_list_of_int (msg:string) (b:bytes) : unit =
  Test.log (msg, (Bytes.unpack b : int list option))

let _test_unpack_pair (msg:string) (b:bytes) : unit =
  Test.log (msg, (Bytes.unpack b : (string * int) option))

let test_unpack_bytes =
  let () = _test_unpack_bytes "test unpack arbitrary bytes" 0x050a0000000a0974657374207061636b in
  let () = _test_unpack_bytes "test unpack bytes 00" 0x050a0000000100 in
  ()

let test_string =
  let () = _test_unpack_string "test unpack arbitrary string" 0x0501000000097061636b2074657374 in
  let () = _test_unpack_string "test unpack empty string" 0x050100000000 in
  ()

let test_int =
  let () = _test_unpack_int "test unpack arbitrary postive int" 0x050001 in
  let () = _test_unpack_int "test unpack arbitrary negative int" 0x050041 in
  let () = _test_unpack_int "test unpack int 0" 0x050000 in
  ()

let test_list =
  let () = _test_unpack_list_of_int "test unpack empty list of int" 0x050200000000 in
  let () = _test_unpack_list_of_int "test unpack list of int" 0x0502000000080001000200030004 in
  let () = Test.log ("test unpack empty list of string", (Bytes.unpack 0x050200000000 : string list option)) in
  let () = Test.log ("test unpack list of pair", (Bytes.unpack 0x0502000000180707000101000000036f6e6507070002010000000374776f : (int*string) list option)) in
  ()

let test_pair =
  let () = _test_unpack_pair "test unpack pair" 0x05070701000000047465737400a401 in
  ()
