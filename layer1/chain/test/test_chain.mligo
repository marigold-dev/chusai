#include "../src/chain.mligo"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"


let _tbi () = Unit.fail_with "TBI"
let empty_chain = 
    { max_index = 0n 
    ; batches = (Big_map.empty : (index, batch) big_map)
    ; children = (Big_map.empty : (index, index list) big_map)
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
    { max_index = 1n 
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
    { max_index = 1n 
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
    { max_index = 1n 
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
    { max_index = 1n 
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

(* Creation of test suite *)
let suite = Unit.make_suite
"Chain"
"Test suite of batch storage lib"
[  Unit.make_test "add one then delete" "test sending first block"  _test_remove_one           
;  Unit.make_test "add two" "sending a son, deleting the father" _test_remove_father            
;  Unit.make_test "add three: siblings" "sending two children for first block, deleting father" _test_remove_twins                   
;  Unit.make_test "add three: grand-children" "one <- two <- three, delete one" _test_remove_grand_child           
]