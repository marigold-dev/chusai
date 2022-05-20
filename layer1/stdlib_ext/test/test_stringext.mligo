#include "../src/stdlibext.mligo"
#include "../src/stdlibtestext.mligo"

let test_StringExt_concat_all =
  let _ = TestExt.assert_equals (StringExt.concat_all "," ([] : string list)) "" in
  let _ = TestExt.assert_equals (StringExt.concat_all "," ["1"]) "1" in
  let _ = TestExt.assert_equals (StringExt.concat_all "," ["1"; "2"]) "1,2" in
  unit