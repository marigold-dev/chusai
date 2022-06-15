#include "../src/stdlibext.mligo"
#import "../src/unit_test.mligo" "Unit"


let suite = Unit.make_suite
"Stdlib"
"Test suite of StringExt"
[ Unit.make_test "Concat empty" "StringExt.concat_all on an empty list" (fun () -> Unit.assert_equals (StringExt.concat_all "," ([] : string list)) "" "Should be empty")
; Unit.make_test "Concat single" "StringExt.concat_all on 1 element" (fun () -> Unit.assert_equals (StringExt.concat_all "," ["1"]) "1" "Should not add comma")
; Unit.make_test "Concat list" "StringExt.concat_all on list" (fun () -> Unit.assert_equals (StringExt.concat_all "," ["1"; "2"]) "1,2" "Should concat add one comma")
]