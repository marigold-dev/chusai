#include "../src/stdlibext.mligo"
#import "../src/atomic_test.mligo" "Atom"


let suite = Atom.make_suite
"Stdlib: Test suite of StringExt"
[ Atom.make_test "Concat empty" "StringExt.concat_all on an empty list" (fun () -> Atom.assert_equals (StringExt.concat_all "," ([] : string list)) "" "Should be empty")
; Atom.make_test "Concat single" "StringExt.concat_all on 1 element" (fun () -> Atom.assert_equals (StringExt.concat_all "," ["1"]) "1" "Should not add comma")
; Atom.make_test "Concat list" "StringExt.concat_all on list" (fun () -> Atom.assert_equals (StringExt.concat_all "," ["1"; "2"]) "1,2" "Should concat add one comma")
]