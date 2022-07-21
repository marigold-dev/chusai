
#include "../src/chain.mligo"
#include "utils.mligo"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#import "../../stdlib_ext/src/result.mligo" "Stdlib_Result"

let timed_block (s: string) (index : index) (parent:index) (level:nat) : block =
    let b : block = block index parent level in
    let timestamp : timestamp = ( "2000-01-01t10:10:10Z" : timestamp) in
    {b with date_of_proposition = timestamp}

let _tbi () = Unit.fail_with "TBI"

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

let _test_get_finalize_candidate_1_and_child = _tbi
let _test_get_finalize_candidate_1_and_non_empty_chain = _tbi
let _test_get_finalize_candidate_1_and_child_non_empty_chain = _tbi
let _test_get_finalize_candidate_2_children                      = _tbi     
(* Creation of test suite *)
let suite = Unit.make_suite
"Chain: finalization"
"Test suite of block storage lib"
[  Unit.make_test "get finalize candidate: 1 block, empty chain" "test finding the first fanalization candidate"  _test_get_finalize_candidate_1
;  Unit.make_test "get finalize candidate: 2 blocks, empty chain" "test finding the first fanalization candidate when there are 1 block and its child"  _test_get_finalize_candidate_1_and_child
;  Unit.make_test "get finalize candidate: 1 blocks, non empty chain" "test finding the first fanalization candidate when there are 1 block and 1 finalized"  _test_get_finalize_candidate_1_and_non_empty_chain
;  Unit.make_test "get finalize candidate: 2 blocks, non empty chain" "test finding the first fanalization candidate when there are 1 block and its child, and 1 finalized"  _test_get_finalize_candidate_1_and_child_non_empty_chain
;  Unit.make_test "get finalize candidate: 2 siblings, non empty chain" "test finding the first fanalization candidate when there are 1 finalized and two children"  _test_get_finalize_candidate_2_children
]