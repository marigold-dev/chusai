(*
   > ligo run test gen_test_data_for_add.mligo
*)
#include "add.mligo"

(* produce bytes by packing add.tz for unit test: test_pack_chusai_contract *)
let test_pack_add_main =
  Test.log ("Add.main", Bytes.pack main)

(* produce bytes for unit test: test_exec_chusai_contract *)
let test_pack_storage =
  Test.log ("Pack storage", Bytes.pack 1n)

let test_pack_arg =
  Test.log ("Pack arg", Bytes.pack 2n)

let test_result =
  Test.log ("Result", Bytes.pack (Some (Bytes.pack 3n)))
