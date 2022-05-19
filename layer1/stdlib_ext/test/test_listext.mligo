#include "../src/stdlibext.mligo"
#include "../src/stdlibtestext.mligo"

let empty_int_list = ([] : int list)

let test_ListExt_reverse =
  let _ = TestExt.assert_equals(ListExt.reverse empty_int_list) (empty_int_list) in
  let _ = TestExt.assert_equals(ListExt.reverse [1;2;3]) ([3;2;1]) in
  unit

let test_ListExt_concat =
  let _ = TestExt.assert_equals (ListExt.concat empty_int_list [1;2;3]) ([1;2;3]) in
  let _ = TestExt.assert_equals (ListExt.concat [1;2;3] empty_int_list) ([1;2;3]) in
  let _ = TestExt.assert_equals (ListExt.concat [1;2;3] [4;5;6]) ([1;2;3;4;5;6]) in
  unit

let test_ListExt_join =
  let _ = TestExt.assert_equals (ListExt.join ([] : int list list)) empty_int_list in
  let _ = TestExt.assert_equals (ListExt.join [empty_int_list; empty_int_list]) empty_int_list in 
  let _ = TestExt.assert_equals (ListExt.join [[1;2;3]; empty_int_list]) [1;2;3] in 
  let _ = TestExt.assert_equals (ListExt.join [[1;2;3]; [4;5;6]]) [1;2;3;4;5;6] in 
  unit

let test_ListExt_bind = 
  let _ = TestExt.assert_equals (ListExt.bind (fun (x : int) -> [1;2;3;4]) empty_int_list) empty_int_list in
  let _ = TestExt.assert_equals (ListExt.bind (fun (x : int) -> [abs x; abs x]) [1;2;3]) [1n;1n;2n;2n;3n;3n] in
  unit

let test_ListExt_find =
  let _ = TestExt.assert_equals (ListExt.find (fun (x : int) -> x = 0) empty_int_list) (None : int option) in 
  let _ = TestExt.assert_equals (ListExt.find (fun (x : int) -> x = 0) [3;2;1]) (None : int option) in 
  let _ = TestExt.assert_equals (ListExt.find (fun (x : int) -> x = 0) [3;2;1;0]) (Some 0) in 
  unit

let test_ListExt_filter =
  let _ = TestExt.assert_equals (ListExt.filter (fun (x : int) -> x > 1) empty_int_list) empty_int_list in 
  let _ = TestExt.assert_equals (ListExt.filter (fun (x : int) -> x > 1) [0;1]) empty_int_list in 
  let _ = TestExt.assert_equals (ListExt.filter (fun (x : int) -> x > 1) [3;2;1;0]) [3;2] in 
  let _ = TestExt.assert_equals (ListExt.filter (fun (x : int) -> x > 1) [2;1;0]) [2] in 
  unit

let test_ListExt_cat_options =
  let _ = TestExt.assert_equals (ListExt.cat_options ([] : int option list)) empty_int_list in
  let _ = TestExt.assert_equals (ListExt.cat_options [(None : int option); (None : int option)]) empty_int_list in 
  let _ = TestExt.assert_equals (ListExt.cat_options [Some 1; Some 2; Some 3]) [1;2;3] in 
  let _ = TestExt.assert_equals (ListExt.cat_options [Some 1; (None : int option); Some 3]) [1;3] in 
  unit

let test_ListExt_sequence_options =
  let _ = TestExt.assert_equals (ListExt.sequence_options ([] : int option list)) (Some empty_int_list) in
  let _ = TestExt.assert_equals (ListExt.sequence_options [(None : int option); (None : int option)]) (None : int list option) in 
  let _ = TestExt.assert_equals (ListExt.sequence_options [Some 1; Some 2; Some 3]) (Some [1;2;3]) in 
  let _ = TestExt.assert_equals (ListExt.sequence_options [Some 1; (None : int option); Some 3]) (None : int list option) in 
  unit

let test_ListExt_intercalate =
  let _ = TestExt.assert_equals (ListExt.intercalate 0 empty_int_list) empty_int_list in
  let _ = TestExt.assert_equals (ListExt.intercalate 0 [1]) [1] in
  let _ = TestExt.assert_equals (ListExt.intercalate 0 [1;2]) [1;0;2] in
  unit