#include "../src/chain.mligo"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"


let empty_chain = 
    { max_index = 0n 
    ; batches = (Big_map.empty : (index, batch) big_map)
    ; children = (Big_map.empty : (index, index list) big_map)
    ; latest_finalized = 0n
    }

let batch (parent:index) (level:nat) : batch = 
        {  parent = parent
        ;  level = level
        ;  hash = 0x0101
        ;  proposer = ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)
        }

let _test_remove_one () = 
    let first = batch 0n 0n in
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; batches = Big_map.literal [(1n, first)]
    ; children = (Big_map.empty : (index, index list) big_map)
    } in
    let new_chain = remove_batch (1n, chain) in
    Unit.and_list 
    [  Unit.assert_equals (None : batch option) (Big_map.find_opt 1n new_chain.batches) "batch should not be stored anymore"
    ;  Unit.assert_equals 1n new_chain.max_index "max_index did not change"
    ] 

let _test_remove_father () =
    let first = batch 0n 0n in
    let second = batch 1n 0n in
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; batches = Big_map.literal [(1n, first) ; (2n, second)]
    ; children = Big_map.literal [(1n, [2n])]
    } in
    let new_chain = remove_batch (1n, chain) in
    Unit.and_list 
    [  Unit.assert_equals (None : batch option) (Big_map.find_opt 1n new_chain.batches) "batch 1 should not be stored anymore"
    ;  Unit.assert_equals (None : batch option) (Big_map.find_opt 2n new_chain.batches) "batch 2 should not be stored anymore"
    ;  Unit.assert_equals 1n new_chain.max_index "max_index did not change"
    ] 

let _test_remove_twins () =
    let first = batch 0n 0n in
    let second = batch 1n 0n in
    let third = batch 1n 0n in
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; batches = Big_map.literal [(1n, first) ; (2n, second) ; (3n, third)]
    ; children = Big_map.literal [(1n, [3n;2n])]
    } in
    let new_chain = remove_batch (1n, chain) in
    Unit.and_list 
    [  Unit.assert_equals (None : batch option) (Big_map.find_opt 1n new_chain.batches) "batch 1 should not be stored anymore"
    ;  Unit.assert_equals (None : batch option) (Big_map.find_opt 2n new_chain.batches) "batch 2 should not be stored anymore"
    ;  Unit.assert_equals (None : batch option) (Big_map.find_opt 3n new_chain.batches) "batch 3 should not be stored anymore"
    ;  Unit.assert_equals 1n new_chain.max_index "max_index did not change"
    ] 

let _test_remove_grand_child () =
    let first = batch 0n 0n in
    let second = batch 1n 0n in
    let third = batch 2n 0n in
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; batches = Big_map.literal [(1n, first) ; (2n, second) ; (3n, third)]
    ; children = Big_map.literal [(1n, [2n]) ; (2n, [3n])]
    } in
    let new_chain = remove_batch (1n, chain) in
    Unit.and_list 
    [  Unit.assert_equals (None : batch option) (Big_map.find_opt 1n new_chain.batches) "batch 1 should not be stored anymore"
    ;  Unit.assert_equals (None : batch option) (Big_map.find_opt 2n new_chain.batches) "batch 2 should not be stored anymore"
    ;  Unit.assert_equals (None : batch option) (Big_map.find_opt 3n new_chain.batches) "batch 3 should not be stored anymore"
    ;  Unit.assert_equals 1n new_chain.max_index "max_index did not change"
    ] 


let _test_receive_first_block () = 
    let first = batch 0n 10n in
    let chain = empty_chain in
    let new_chain = store_batch (first, chain) in
    Unit.and_lazy_list 
    [  fun () -> Unit.assert_not_none new_chain "store should have succeeded"
    ;  fun () -> let new_chain = Option.unopt new_chain in Unit.assert_equals 1n (new_chain.max_index) "max_index should be 1"
    ;  fun () -> let new_chain = Option.unopt new_chain in Unit.assert_equals (Some first) (get_batch (1n, new_chain)) "the batch should have been stored"
    ]



let _test_receive_son () = 
    let first = batch 0n 10n in
    let second = batch 1n 20n in
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; batches = (Big_map.literal [(1n,first)])
    ; children = (Big_map.empty : (index, index list) big_map)
    } in
    let new_chain = store_batch (second, chain) in
    Unit.and_lazy_list 
    [  fun () -> Unit.assert_not_none new_chain "store should have succeeded"
    ;  fun () -> let new_chain = Option.unopt new_chain in Unit.assert_equals 2n (new_chain.max_index) "max_index should be 2"
    ;  fun () -> let new_chain = Option.unopt new_chain in Unit.assert_equals (Some first) (get_batch (1n, new_chain)) "the first batch should have been stored"
    ;  fun () -> let new_chain = Option.unopt new_chain in Unit.assert_equals (Some second) (get_batch (2n, new_chain)) "the second batch should have been stored"
    ;  fun () -> let new_chain = Option.unopt new_chain in Unit.assert_equals (Some [2n]) (get_children (1n, new_chain)) "second batch is a child of first"
    ]

let _test_receive_siblings () = 
    let first = batch 0n 10n in
    let second = batch 1n 20n in
    let third = batch 1n 20n in
    let chain = 
    { empty_chain with
      max_index = 2n 
    ; batches = (Big_map.literal [(1n, first) ; (2n, second)])
    ; children = (Big_map.literal [(1n, [2n])])
    } in
    let new_chain = store_batch (third, chain) in
    Unit.and_lazy_list 
    [  fun () -> Unit.assert_not_none new_chain "store should have succeeded"
    ;  fun () -> let new_chain = Option.unopt new_chain in Unit.assert_equals 3n (new_chain.max_index) "max_index should be 3"
    ;  fun () -> let new_chain = Option.unopt new_chain in Unit.assert_equals (Some first) (get_batch (1n, new_chain)) "the first batch should have been stored"
    ;  fun () -> let new_chain = Option.unopt new_chain in Unit.assert_equals (Some second) (get_batch (2n, new_chain)) "the second batch should have been stored"
    ;  fun () -> let new_chain = Option.unopt new_chain in Unit.assert_equals (Some third) (get_batch (3n, new_chain)) "the third batch should have been stored"
    ;  fun () -> let new_chain = Option.unopt new_chain in Unit.assert_equals (Some [3n ; 2n]) (get_children (1n, new_chain)) "second and third batch are children of first"
    ]

let _test_receive_orphan  () = 
    let first = batch 0n 10n in
    let second = batch 3n 20n in
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; batches = (Big_map.literal [(1n,first)])
    ; children = (Big_map.empty : (index, index list) big_map)
    } in
    let new_chain = store_batch (second, chain) in
    Unit.assert_equals (None : chain option) new_chain "store should have failed"


(* Creation of test suite *)
let suite = Unit.make_suite
"Chain"
"Test suite of batch storage lib"
[  Unit.make_test "add one then delete" "test sending first block"  _test_remove_one           
;  Unit.make_test "add two" "sending a son, deleting the father" _test_remove_father            
;  Unit.make_test "add three: siblings" "sending two children for first block, deleting father" _test_remove_twins                   
;  Unit.make_test "add three: grand-children" "one <- two <- three, delete one" _test_remove_grand_child 
;  Unit.make_test "First block: success" "test sending first block"  _test_receive_first_block           
;  Unit.make_test "Second block: success" "sending a son" _test_receive_son            
;  Unit.make_test "Third block: success" "sending two children for first block" _test_receive_siblings                   
;  Unit.make_test "Second block: failure" "sending an orphan" _test_receive_orphan           
]