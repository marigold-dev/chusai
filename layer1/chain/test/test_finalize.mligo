
#include "../src/chain.mligo"
#include "utils.mligo"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#import "../../stdlib_ext/src/result.mligo" "Stdlib_Result"

let _test_get_finalize_candidate_1 () =
    let block = block 1n 0n 10n in 
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; blocks = Big_map.literal [(1n, block)]
    ; children = Big_map.literal [(0n, [1n])]
    } in
    let candidate = get_finalization_candidate chain in
    Unit.and_lazy_list 
    [  fun () -> Unit.assert_ (Result.is_ok candidate) "a candidate should have been found"
    ;  fun () -> Unit.assert_equals ((Ok block) : (block, chain_error) result ) candidate "the first block should have been returned"
    ]

let _test_get_finalize_candidate_1_and_child () = 
    let first = block 1n 0n 10n in 
    let second = block 2n 1n 10n in 
    let chain = 
    { empty_chain with
      max_index = 2n 
    ; blocks = Big_map.literal [(1n, first) ; (2n, second)]
    ; children = Big_map.literal [(0n, [1n]) ; (1n, [2n])]
    } in
    let candidate = get_finalization_candidate chain in
    Unit.and_lazy_list 
    [  fun () -> Unit.assert_ (Result.is_ok candidate) "a candidate should have been found"
    ;  fun () -> Unit.assert_equals ((Ok first) : (block, chain_error) result ) candidate "the first block should have been returned"
    ]

let _test_get_finalize_candidate_1_and_non_empty_chain () =
    let first = block 1n 0n 10n in 
    let second = block 2n 1n 10n in 
    let chain = 
    { empty_chain with
      max_index = 2n 
    ; blocks = Big_map.literal [(1n, first) ; (2n, second)]
    ; children = Big_map.literal [(0n, [1n]) ; (1n, [2n])]
    ; latest_finalized = 1n
    } in
    let candidate = get_finalization_candidate chain in
    Unit.and_lazy_list 
    [  fun () -> Unit.assert_ (Result.is_ok candidate) "a candidate should have been found"
    ;  fun () -> Unit.assert_equals ((Ok second) : (block, chain_error) result ) candidate "the second block should have been returned"
    ]


let _test_get_finalize_candidate_1_and_child_non_empty_chain () =
    let first = block 1n 0n 10n in 
    let second = block 2n 1n 10n in 
    let third = block 3n 2n 10n in 
    let chain = 
    { empty_chain with
      max_index = 2n 
    ; blocks = Big_map.literal [(1n, first) ; (2n, second) ; (3n, third)]
    ; children = Big_map.literal [(0n, [1n]) ; (1n, [2n]) ; (2n, [3n])]
    ; latest_finalized = 1n
    } in
    let candidate = get_finalization_candidate chain in
    Unit.and_lazy_list 
    [  fun () -> Unit.assert_ (Result.is_ok candidate) "a candidate should have been found"
    ;  fun () -> Unit.assert_equals ((Ok second) : (block, chain_error) result ) candidate "the second block should have been returned"
    ]

let _test_get_finalize_candidate_2_children () =
    let first = block 1n 0n 10n in 
    let second = block 2n 1n 10n in 
    let third = block 3n 1n 10n in 
    let chain = 
    { empty_chain with
      max_index = 2n 
    ; blocks = Big_map.literal [(1n, first) ; (2n, second) ; (3n, third)]
    ; children = Big_map.literal [(0n, [1n]) ; (1n, [3n ; 2n])]
    ; latest_finalized = 1n
    } in
    let candidate = get_finalization_candidate chain in
    Unit.and_lazy_list 
    [  fun () -> Unit.assert_ (Result.is_ok candidate) "a candidate should have been found"
    ;  fun () -> Unit.assert_equals ((Ok second) : (block, chain_error) result ) candidate "the second block should have been returned"
    ]    

let _test_get_finalize_candidate_empty_no_candidate () =
    let candidate = get_finalization_candidate empty_chain in
    Unit.assert_equals (Error No_candidate : (block, chain_error) result) candidate "no candidate should have been found"

let _test_get_finalize_candidate_non_empty_no_candidate () =
    let block = block 1n 0n 10n in 
    let chain = 
    { empty_chain with
      max_index = 1n 
    ; blocks = Big_map.literal [(1n, block)]
    ; children = Big_map.literal [(0n, [1n])]
    ; latest_finalized = 1n
    } in
    let candidate = get_finalization_candidate chain in
    Unit.assert_equals (Error No_candidate : (block, chain_error) result) candidate "no candidate should have been found"
    
let _test_is_old_enough_success () =
    let block = {prototype_block with date_of_proposition = ("2000-01-01t10:10:10Z" : timestamp)} in
    let chain = empty_chain in
    let today = ("2050-01-01t10:10:10Z" : timestamp) in
    Unit.assert_ (is_old_enough (block, chain, today) ) "block should be old enough"

let _test_is_old_enough_failure () =
    let block = {prototype_block with date_of_proposition = ("2000-01-01t10:10:10Z" : timestamp)} in
    let chain = empty_chain in
    let today = ("1999-01-01t10:10:10Z" : timestamp) in
    Unit.assert_ (not (is_old_enough (block, chain, today))) "block should not be old enough"

(* Creation of test suite *)
let suite = Unit.make_suite
"Chain: finalization"
"Test suite of block storage lib"
[  Unit.make_test "get finalize candidate: 1 block, empty chain" "test finding the first fanalization candidate"  _test_get_finalize_candidate_1
;  Unit.make_test "get finalize candidate: 2 blocks, empty chain" "test finding the first fanalization candidate when there are 1 block and its child"  _test_get_finalize_candidate_1_and_child
;  Unit.make_test "get finalize candidate: 1 blocks, non empty chain" "test finding the first fanalization candidate when there are 1 finalized and its child"  _test_get_finalize_candidate_1_and_non_empty_chain
;  Unit.make_test "get finalize candidate: 2 blocks, non empty chain" "test finding the first fanalization candidate when there are 1 block and its child, and 1 finalized"  _test_get_finalize_candidate_1_and_child_non_empty_chain
;  Unit.make_test "get finalize candidate: 2 siblings, non empty chain" "test finding the first fanalization candidate when there are 1 finalized and two children"  _test_get_finalize_candidate_2_children
;  Unit.make_test "get finalize candidate: 2 siblings, non empty chain" "test finding the first fanalization candidate when there are 1 finalized and two children"  _test_get_finalize_candidate_2_children
;  Unit.make_test "get finalize candidate: empty chain, no candidate" "test finding the first fanalization candidate when there are no blocks"  _test_get_finalize_candidate_empty_no_candidate
;  Unit.make_test "get finalize candidate: non empty chain, no candidate" "test finding the first fanalization candidate when there are 1 finalized and no child"  _test_get_finalize_candidate_non_empty_no_candidate
;  Unit.make_test "is old enough: success" "block in 2000 against date in the futur (today is 2050)" _test_is_old_enough_success
;  Unit.make_test "is old enough: failure" "block in 2000 against date in the past (today is 1999)" _test_is_old_enough_failure
]
