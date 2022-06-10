#include "../src/stdlibext.mligo"
#import "../src/unit_test.mligo" "Unit"

let empty_int_list = ([] : int list)

let _test_ListExt_reverse () = 
  Unit.and
  (Unit.assert_equals (ListExt.reverse empty_int_list) (empty_int_list) "")
  (Unit.assert_equals (ListExt.reverse [1;2;3]) ([3;2;1]) "")

let _test_ListExt_concat () =
  Unit.and_list 
  [ Unit.assert_equals (ListExt.concat empty_int_list [1;2;3]) ([1;2;3]) ""
  ; Unit.assert_equals (ListExt.concat [1;2;3] empty_int_list) ([1;2;3]) ""
  ; Unit.assert_equals (ListExt.concat [1;2;3] [4;5;6]) ([1;2;3;4;5;6]) ""
  ]


let _test_ListExt_join () = 
  Unit.and_list 
  [ Unit.assert_equals (ListExt.join ([] : int list list)) empty_int_list ""
  ; Unit.assert_equals (ListExt.join [empty_int_list; empty_int_list]) empty_int_list ""
  ; Unit.assert_equals (ListExt.join [[1;2;3]; empty_int_list]) [1;2;3] ""
  ; Unit.assert_equals (ListExt.join [[1;2;3]; [4;5;6]]) [1;2;3;4;5;6] ""
  ]
let _test_ListExt_bind () = 
  Unit.and
   (Unit.assert_equals (ListExt.bind (fun (x : int) -> [1;2;3;4]) empty_int_list) empty_int_list  "")
   (Unit.assert_equals (ListExt.bind (fun (x : int) -> [abs x; abs x]) [1;2;3]) [1n;1n;2n;2n;3n;3n] "")

let _test_ListExt_find () = 
  Unit.and_list 
  [ Unit.assert_equals (ListExt.find (fun (x : int) -> x = 0) empty_int_list) (None : int option) "" 
  ; Unit.assert_equals (ListExt.find (fun (x : int) -> x = 0) [3;2;1]) (None : int option) "" 
  ; Unit.assert_equals (ListExt.find (fun (x : int) -> x = 0) [3;2;1;0]) (Some 0) "" 
  ]

let _test_ListExt_filter () = 
  Unit.and_list 
  [ Unit.assert_equals (ListExt.filter (fun (x : int) -> x > 1) empty_int_list) empty_int_list "" 
  ; Unit.assert_equals (ListExt.filter (fun (x : int) -> x > 1) [0;1]) empty_int_list "" 
  ; Unit.assert_equals (ListExt.filter (fun (x : int) -> x > 1) [3;2;1;0]) [3;2] "" 
  ; Unit.assert_equals (ListExt.filter (fun (x : int) -> x > 1) [2;1;0]) [2] "" 
  ]

let _test_ListExt_cat_options () = 
  Unit.and_list 
  [ Unit.assert_equals (ListExt.cat_options ([] : int option list)) empty_int_list ""
  ; Unit.assert_equals (ListExt.cat_options [(None : int option); (None : int option)]) empty_int_list "" 
  ; Unit.assert_equals (ListExt.cat_options [Some 1; Some 2; Some 3]) [1;2;3] "" 
  ; Unit.assert_equals (ListExt.cat_options [Some 1; (None : int option); Some 3]) [1;3] "" 
  ]

let _test_ListExt_sequence_options () = 
  Unit.and_list 
  [ Unit.assert_equals (ListExt.sequence_options ([] : int option list)) (Some empty_int_list) ""
  ; Unit.assert_equals (ListExt.sequence_options [(None : int option); (None : int option)]) (None : int list option) "" 
  ; Unit.assert_equals (ListExt.sequence_options [Some 1; Some 2; Some 3]) (Some [1;2;3]) "" 
  ; Unit.assert_equals (ListExt.sequence_options [Some 1; (None : int option); Some 3]) (None : int list option) "" 
  ]

let _test_ListExt_intercalate () = 
  Unit.and_list 
  [ Unit.assert_equals (ListExt.intercalate 0 empty_int_list) empty_int_list ""
  ; Unit.assert_equals (ListExt.intercalate 0 [1]) [1] ""
  ; Unit.assert_equals (ListExt.intercalate 0 [1;2]) [1;0;2] ""
  ]



let suite = Unit.make_suite
  "stdlib"
  "Test suite for ListExt"
  [ Unit.make_test "ListExt" "simple tests: reverse" _test_ListExt_reverse 
  ; Unit.make_test "ListExt" "simple tests: concat" _test_ListExt_concat 
  ; Unit.make_test "ListExt" "simple tests: join" _test_ListExt_join 
  ; Unit.make_test "ListExt" "simple tests: bind" _test_ListExt_bind 
  ; Unit.make_test "ListExt" "simple tests: find" _test_ListExt_find 
  ; Unit.make_test "ListExt" "simple tests: filter" _test_ListExt_filter 
  ; Unit.make_test "ListExt" "simple tests: cat_options" _test_ListExt_cat_options 
  ; Unit.make_test "ListExt" "simple tests: sequence options" _test_ListExt_sequence_options 
  ; Unit.make_test "ListExt" "simple tests intercalate" _test_ListExt_intercalate 
  ]