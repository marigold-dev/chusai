#include "../src/chain.mligo"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#import "../../stdlib_ext/src/result.mligo" "Stdlib_Result"


let empty_chain : chain = 
    { max_index = 0n 
    ; blocks = (Big_map.empty : (index, block) big_map)
    ; children = (Big_map.empty : (index, index list) big_map)
    }

let block (parent:index) (level:nat) : block = 
        {  parent = parent
        ;  level = level
        ;  hash = 0x0101
        ;  proposer = ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)
        }

let _test_remove_one () = 
    let first = block 0n 0n in
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; blocks = Big_map.literal [(1n, first)]
    ; children = (Big_map.empty : (index, index list) big_map)
    } in
    let new_chain = remove_block (1n, chain) in
    Unit.and_list 
    [  Unit.assert_equals (None : block option) (Big_map.find_opt 1n new_chain.blocks) "block should not be stored anymore"
    ;  Unit.assert_equals 1n new_chain.max_index "max_index did not change"
    ] 

let _test_remove_father () =
    let first = block 0n 0n in
    let second = block 1n 0n in
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; blocks = Big_map.literal [(1n, first) ; (2n, second)]
    ; children = Big_map.literal [(1n, [2n])]
    } in
    let new_chain = remove_block (1n, chain) in
    Unit.and_list 
    [  Unit.assert_equals (None : block option) (Big_map.find_opt 1n new_chain.blocks) "block 1 should not be stored anymore"
    ;  Unit.assert_equals (None : block option) (Big_map.find_opt 2n new_chain.blocks) "block 2 should not be stored anymore"
    ;  Unit.assert_equals 1n new_chain.max_index "max_index did not change"
    ] 

let _test_remove_twins () =
    let first = block 0n 0n in
    let second = block 1n 0n in
    let third = block 1n 0n in
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; blocks = Big_map.literal [(1n, first) ; (2n, second) ; (3n, third)]
    ; children = Big_map.literal [(1n, [3n;2n])]
    } in
    let new_chain = remove_block (1n, chain) in
    Unit.and_list 
    [  Unit.assert_equals (None : block option) (Big_map.find_opt 1n new_chain.blocks) "block 1 should not be stored anymore"
    ;  Unit.assert_equals (None : block option) (Big_map.find_opt 2n new_chain.blocks) "block 2 should not be stored anymore"
    ;  Unit.assert_equals (None : block option) (Big_map.find_opt 3n new_chain.blocks) "block 3 should not be stored anymore"
    ;  Unit.assert_equals 1n new_chain.max_index "max_index did not change"
    ] 

let _test_remove_grand_child () =
    let first = block 0n 0n in
    let second = block 1n 0n in
    let third = block 2n 0n in
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; blocks = Big_map.literal [(1n, first) ; (2n, second) ; (3n, third)]
    ; children = Big_map.literal [(1n, [2n]) ; (2n, [3n])]
    } in
    let new_chain = remove_block (1n, chain) in
    Unit.and_list 
    [  Unit.assert_equals (None : block option) (Big_map.find_opt 1n new_chain.blocks) "block 1 should not be stored anymore"
    ;  Unit.assert_equals (None : block option) (Big_map.find_opt 2n new_chain.blocks) "block 2 should not be stored anymore"
    ;  Unit.assert_equals (None : block option) (Big_map.find_opt 3n new_chain.blocks) "block 3 should not be stored anymore"
    ;  Unit.assert_equals 1n new_chain.max_index "max_index did not change"
    ] 


let _test_receive_first_block () = 
    let first = block 0n 10n in
    let chain = empty_chain in
    let new_chain = store_block (first, chain) in
    Unit.and_lazy_list 
    [  fun () -> Unit.assert_ (Stdlib_Result.is_ok new_chain) "store should have succeeded"
    ;  fun () -> let new_chain = Stdlib_Result.get_ok new_chain in Unit.assert_equals 1n (new_chain.max_index) "max_index should be 1"
    ;  fun () -> let new_chain = Stdlib_Result.get_ok new_chain in Unit.assert_equals (Some first) (get_block (1n, new_chain)) "the block should have been stored"
    ]



let _test_receive_son () = 
    let first = block 0n 10n in
    let second = block 1n 20n in
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; blocks = (Big_map.literal [(1n,first)])
    ; children = (Big_map.empty : (index, index list) big_map)
    } in
    let new_chain = store_block (second, chain) in
    Unit.and_lazy_list 
    [  fun () -> Unit.assert_ (Stdlib_Result.is_ok new_chain) "store should have succeeded"
    ;  fun () -> let new_chain = Stdlib_Result.get_ok new_chain in Unit.assert_equals 2n (new_chain.max_index) "max_index should be 2"
    ;  fun () -> let new_chain = Stdlib_Result.get_ok new_chain in Unit.assert_equals (Some first) (get_block (1n, new_chain)) "the first block should have been stored"
    ;  fun () -> let new_chain = Stdlib_Result.get_ok new_chain in Unit.assert_equals (Some second) (get_block (2n, new_chain)) "the second block should have been stored"
    ;  fun () -> let new_chain = Stdlib_Result.get_ok new_chain in Unit.assert_equals (Some [2n]) (get_children (1n, new_chain)) "second block is a child of first"
    ]

let _test_receive_siblings () = 
    let first = block 0n 10n in
    let second = block 1n 20n in
    let third = block 1n 20n in
    let chain = 
    { empty_chain with
      max_index = 2n 
    ; blocks = (Big_map.literal [(1n, first) ; (2n, second)])
    ; children = (Big_map.literal [(1n, [2n])])
    } in
    let new_chain = store_block (third, chain) in
    Unit.and_lazy_list 
    [  fun () -> Unit.assert_ (Stdlib_Result.is_ok new_chain) "store should have succeeded"
    ;  fun () -> let new_chain = Stdlib_Result.get_ok new_chain in Unit.assert_equals 3n (new_chain.max_index) "max_index should be 3"
    ;  fun () -> let new_chain = Stdlib_Result.get_ok new_chain in Unit.assert_equals (Some first) (get_block (1n, new_chain)) "the first block should have been stored"
    ;  fun () -> let new_chain = Stdlib_Result.get_ok new_chain in Unit.assert_equals (Some second) (get_block (2n, new_chain)) "the second block should have been stored"
    ;  fun () -> let new_chain = Stdlib_Result.get_ok new_chain in Unit.assert_equals (Some third) (get_block (3n, new_chain)) "the third block should have been stored"
    ;  fun () -> let new_chain = Stdlib_Result.get_ok new_chain in Unit.assert_equals (Some [3n ; 2n]) (get_children (1n, new_chain)) "second and third block are children of first"
    ]

let _test_receive_orphan  () = 
    let first = block 0n 10n in
    let second = block 3n 20n in
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; blocks = (Big_map.literal [(1n,first)])
    ; children = (Big_map.empty : (index, index list) big_map)
    } in
    let new_chain = store_block (second, chain) in
    Unit.assert_ (Stdlib_Result.is_error new_chain)  "store should have failed"


(* Creation of test suite *)
let suite = Unit.make_suite
"Chain"
"Test suite of block storage lib"
[  Unit.make_test "add one then delete" "test sending first block"  _test_remove_one           
;  Unit.make_test "add two" "sending a son, deleting the father" _test_remove_father            
;  Unit.make_test "add three: siblings" "sending two children for first block, deleting father" _test_remove_twins                   
;  Unit.make_test "add three: grand-children" "one <- two <- three, delete one" _test_remove_grand_child 
;  Unit.make_test "First block: success" "test sending first block"  _test_receive_first_block           
;  Unit.make_test "Second block: success" "sending a son" _test_receive_son            
;  Unit.make_test "Third block: success" "sending two children for first block" _test_receive_siblings                   
;  Unit.make_test "Second block: failure" "sending an orphan" _test_receive_orphan           
]