#include "../src/stdlibext.mligo"
#import "../src/unit_test.mligo" "Unit"


let _test_OptionExt_bind_none ()= 
  let actual = 
    OptionExt.bind 
      (None : int option) 
      (fun (x : int) -> Some x) in
  Unit.assert_equals actual (None : int option) ""

let _test_OptionExt_bind_some ()= 
    let actual = 
      OptionExt.bind 
        (Some 10 : int option) 
        (fun (x : int) -> Some x) in
    Unit.assert_equals actual (Some 10) ""

let _test_OptionExt_bind_hide ()= 
    let actual = 
      OptionExt.bind 
        (Some 10 : int option) 
        (fun (_ : int) -> (None : int option)) in
    Unit.assert_equals actual (None : int option) ""


let suite = Unit.make_suite
"Stdlib"
"Test suite of OptionExt"
[ Unit.make_test "Bind on None" "bind x->x on None" _test_OptionExt_bind_none
; Unit.make_test "Bind on Some" "bind x->x on Some y" _test_OptionExt_bind_some
; Unit.make_test "Bind on Some" "bind x->None on Some y" _test_OptionExt_bind_hide
]