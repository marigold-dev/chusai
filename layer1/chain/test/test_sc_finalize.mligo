#include "../src/chain_sc.mligo"
#include "utils.mligo"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"

type actor = Unit.actor
(* UTILS *)

let send_proposal (actor : actor) (proposal : block_proposal) (chain : originated_chain) = 
    let _ = Test.compile_value (Receive proposal) in 
    let send_block () = Unit.transfer_to_contract_ chain.originated_contract (Receive proposal) bond in
    Unit.act_as actor send_block

(* TESTS *)


// test sending a block
let _test_finalize_first () = 
    let operator, actors = Unit.init_default_at ("2020-01-01t10:10:10Z" : timestamp) in 
    let alice, _bob, _carol = actors in 
    let block_alice  = 
        let b = block 1n 0n 10n  in 
        {b with proposer = alice.address} 
        in
    let init_chain = 
        { empty_chain with
          max_index = 1n 
        ; blocks = Big_map.literal [(1n, block_alice)]
        ; children = Big_map.literal [(0n, [1n])]
        } in
    // sanity check
    let candidate = get_finalization_candidate init_chain in
    let sanity_check = 
        Unit.and_lazy_list 
        [  fun () -> Unit.assert_ (Result.is_ok candidate) "a candidate should have been found"
        ;  fun () -> Unit.assert_equals ((Ok block_alice) : (block, chain_error) result ) candidate "alice's block should be next candidate"
        ] in
    // origination
    let chain : originated_chain = Unit.act_as operator (originate_chain_with init_chain bond)  in
    let alice_initial_balance = Test.get_balance alice.address in

    // act
    let finalize_result = Unit.act_as operator (fun () -> Unit.transfer_to_contract_ chain.originated_contract Finalize bond) in

    //assert
    let storage = Test.get_storage chain.originated_typed_address in
    let alice_new_balance = Test.get_balance alice.address in
    Unit.and_list 
    [  sanity_check    
    ;  Unit.assert_is_ok finalize_result "finalization should have succeeded" 
    ;  Unit.assert_equals 1n storage.latest_finalized "block 1n should have been finalize"
    ;  Unit.assert_equals (alice_initial_balance + bond) alice_new_balance "Alice should have received the reward"
    ]

let _test_finalize_second () =
    // setup
    let operator, actors = Unit.init_default_at ("2020-01-01t10:10:10Z" : timestamp) in 
    let alice, bob, _carol = actors in 
    let block_alice  = 
        let b = block 1n 0n 10n  in 
        {b with proposer = alice.address} 
        in
    let block_bob  = 
        let b = block 2n 1n 20n  in 
        {b with proposer = bob.address} 
        in
    let block_alice_2  = 
        let b = block 3n 2n 10n  in 
        {b with proposer = alice.address} 
        in
    let init_chain = 
        { empty_chain with
          max_index = 3n 
        ; blocks = Big_map.literal [(1n, block_alice) ; (2n, block_bob) ; (3n, block_alice_2)]
        ; children = Big_map.literal [(0n, [1n]) ; (1n, [2n]) ; (2n , [3n])]
        ; latest_finalized = 1n
        } in
    // sanity check
    let candidate = get_finalization_candidate init_chain in
    let sanity_check = 
        Unit.and_lazy_list 
        [  fun () -> Unit.assert_ (Result.is_ok candidate) "a candidate should have been found"
        ;  fun () -> Unit.assert_equals ((Ok block_bob) : (block, chain_error) result ) candidate "bob's block should be next candidate"
        ] in
    // origination
    let chain : originated_chain = Unit.act_as operator (originate_chain_with init_chain bond)  in
    let bob_initial_balance = Test.get_balance bob.address in
    
    // act
    let finalize_result = Unit.act_as operator (fun () -> Unit.transfer_to_contract_ chain.originated_contract Finalize bond) in

    // assert
    let storage = Test.get_storage chain.originated_typed_address in
    let bob_new_balance = Test.get_balance bob.address in
    Unit.and_list 
    [  sanity_check
    ;  Unit.assert_is_ok finalize_result "finalization should have succeeded"
    ;  Unit.assert_equals 2n storage.latest_finalized "block 2n should have been finalize"
    ;  Unit.assert_equals (bob_initial_balance + bond) bob_new_balance "Bob should have received the reward"
    ]

let _test_finalize_orphan () = 
    let operator, actors = Unit.init_default_at ("2020-01-01t10:10:10Z" : timestamp) in 
    let alice, _bob, _carol = actors in 
    let block_alice  = 
        let b = block 1n 0n 10n  in 
        {b with proposer = alice.address} 
        in
    let init_chain = 
        { empty_chain with
          max_index = 1n 
        ; blocks = Big_map.literal [(1n, block_alice)]
        ; children = Big_map.literal [(0n, [1n])]
        ; latest_finalized = 1n
        } in
    // sanity check
    let candidate = get_finalization_candidate init_chain in
    let sanity_check = 
        Unit.and_lazy_list 
        [  fun () -> Unit.assert_ (Result.is_error candidate) "no candidate should have been found"
        ] in
    // origination
    let chain : originated_chain = Unit.act_as operator (originate_chain_with init_chain bond)  in
    let alice_initial_balance = Test.get_balance alice.address in

    // act
    let finalize_result = Unit.act_as operator (fun () -> Unit.transfer_to_contract_ chain.originated_contract Finalize bond) in

    //assert
    let storage = Test.get_storage chain.originated_typed_address in
    let alice_new_balance = Test.get_balance alice.address in
    Unit.and_list 
    [  sanity_check    
    ;  Unit.assert_rejected_at finalize_result chain.originated_address "finalization should have failed" 
    ;  Unit.assert_equals 1n storage.latest_finalized "block 1n should have stayed finalize"
    ;  Unit.assert_equals (alice_initial_balance) alice_new_balance "Alice should have received the reward"
    ]

(* Creation of test suite *)
let suite = Unit.make_suite
"Chain_sc : finalize"
"Test suite of finalisation of blocks"
[  Unit.make_test "finalize first block" "test finalizing the first block ever"  _test_finalize_first          
;  Unit.make_test "finalize second block" "test finalizing a second block, child of current last finalized block" _test_finalize_second                        
;  Unit.make_test "finalize block with no proposer" "test finalizing a block with no correct proposal" _test_finalize_orphan           
]
