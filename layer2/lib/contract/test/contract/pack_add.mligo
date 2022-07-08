(* produce bytes by packing add.tz for unit test
   > ligo run test pack_add.mligo
*)
#include "add.mligo"

let test =
  Test.log (Bytes.pack main)
