#include "../test/chain_sc.mligo"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#include "utils.mligo"
(* UTILS *)

(* TESTS *)


// test sending a block
let _test_receive_first_block () = 
    let operator, actors = Unit.init_default () in 
    let alice, _bob, _carol = actors in 
    let chain : originated_chain = Unit.act_as operator originate_chain  in
    let my_block  = 
        {  prototype_block_proposal with
           parent = 0n
        ;  inbox_level = 0n
        } in 
    let _ = Test.compile_value (Receive my_block) in 
    let send_block () = Unit.transfer_to_contract_ chain.originated_contract (Receive my_block) bond in
    let result = Unit.act_as alice send_block in
    let storage = Test.get_storage chain.originated_typed_address in
    Unit.and_list 
    [  result 
    ;  Unit.assert_equals 1n (storage.max_index) "max_index should be 1"
    ;  let b = (compare_proposal_and_block my_block (get_block (1n, storage))) in
        Unit.assert_ b "the block should have been stored"
    ]


// test sending its son
let _test_receive_son () = 
    // setup
    let operator, actors = Unit.init_default () in 
    let alice, bob, _carol = actors in 
    let chain : originated_chain = Unit.act_as operator originate_chain  in

    // act
    let first_block  = 
        {  prototype_block_proposal with
           parent = 0n
        ;  inbox_level = 0n
        } in 
    let second_block = 
        {  prototype_block_proposal with
           parent = 1n
        ;  inbox_level = 10n
        } in 
    let send_block (block : block_proposal) () = Unit.transfer_to_contract_ chain.originated_contract (Receive block) bond in
    let result_alice = Unit.act_as alice (send_block first_block) in
    let result_bob = Unit.act_as bob (send_block second_block) in

    // assert
    let storage = Test.get_storage chain.originated_typed_address in
    Unit.and_list 
    [  Unit.assert_is_ok result_alice "first block should have succeeded"
    ;  Unit.assert_is_ok result_bob  "second block should have succeeded"
    ;  Unit.assert_equals 2n (storage.max_index) "max_index should be 2"
    ;  Unit.assert_ (compare_proposal_and_block first_block (get_block (1n, storage))) "the first block should have been stored"
    ;  Unit.assert_ (compare_proposal_and_block second_block (get_block (2n, storage))) "the second block should have been stored"
    ;  Unit.assert_equals (Some [2n]) (get_children (1n, storage)) "second block is a child of first"
    ]

// test sending an orphan
let _test_receive_orphan () = 
    // setup
    let operator, actors = Unit.init_default () in 
    let alice, bob, _carol = actors in 
    let chain : originated_chain = Unit.act_as operator originate_chain  in
    
    // act
    let first_block  = 
        {  prototype_block_proposal with
           parent = 0n
        ;  inbox_level = 0n
        } in 
    let second_block = 
        {  prototype_block_proposal with
           parent = 3n
        ;  inbox_level = 10n
        } in 
    let send_block (block : block_proposal) () = Unit.transfer_to_contract_ chain.originated_contract (Receive block) bond in
    let result_alice = Unit.act_as alice (send_block first_block) in
    let result_bob = Unit.act_as bob (send_block second_block) in

    // assert
    let storage = Test.get_storage chain.originated_typed_address in
    Unit.and_list 
    [  Unit.assert_is_ok result_alice "first block should have succeeded"
    ;  Unit.assert_rejected result_bob  "second block should have failed"
    ;  Unit.assert_equals 1n (storage.max_index) "max_index should be 1: the second block failed"
    ;  Unit.assert_ (compare_proposal_and_block  first_block (get_block (1n, storage))) "the block should have been stored"
    ;  Unit.assert_equals (None : index list option) (get_children (1n, storage)) "second block not recorded as a child of first"
    ]


// test sending its son
let _test_receive_siblings () = 
    // setup
    let operator, actors = Unit.init_default () in 
    let alice, bob, _carol = actors in 
    let chain : originated_chain = Unit.act_as operator originate_chain  in

    // act
    let first_block  = 
        {  prototype_block_proposal with
           parent = 0n
        ;  inbox_level = 0n
        } in 
    let second_block = 
        {  prototype_block_proposal with
           parent = 1n
        ;  inbox_level = 10n
        } in 
    let third_block = 
        {  prototype_block_proposal with
           parent = 1n
        ;  inbox_level = 20n
        } in 
    let send_block (block : block_proposal) () = Unit.transfer_to_contract_ chain.originated_contract (Receive block) bond in
    let result_first = Unit.act_as alice (send_block first_block) in
    let result_second = Unit.act_as bob (send_block second_block) in
    let result_third = Unit.act_as bob (send_block third_block) in

    // assert
    let storage = Test.get_storage chain.originated_typed_address in
    Unit.and_list 
    [  Unit.assert_is_ok result_first "first block should have succeeded"
    ;  Unit.assert_is_ok result_second  "second block should have succeeded"
    ;  Unit.assert_is_ok result_third  "third block should have succeeded"
    ;  Unit.assert_equals 3n (storage.max_index) "max_index should be 3"
    ;  Unit.assert_equals (Some [3n ; 2n]) (get_children (1n, storage)) "second block is a child of first"
    ]

let _test_remove_block () =
    let operator, actors = Unit.init_default () in 
    let alice, _bob, _carol = actors in 
    let chain : originated_chain = Unit.act_as operator originate_chain  in
    let alice_balance_at_origination = Test.get_balance alice.address in
    let my_block  = 
        {  prototype_block_proposal with
           parent = 0n
        ;  inbox_level = 0n
        ;  hash = 0x0101
        } in 
    let send_block () = Unit.transfer_to_contract_ chain.originated_contract (Receive my_block) bond in
    let result = Unit.act_as alice send_block in
    let storage = Test.get_storage chain.originated_typed_address in
    let alice_balance_after_proposal = Test.get_balance alice.address in
    let sanity_check = Unit.and_list 
    [  result 
    ;  Unit.assert_ (alice_balance_after_proposal + bond < alice_balance_at_origination) "Alice should have send bond"
    ;  Unit.assert_equals 1n (storage.max_index) "max_index should be 1"
    ;  Unit.assert_ (compare_proposal_and_block  my_block (get_block (1n, storage))) "the block should have been stored"
    ] in
    let remove_block () = Unit.transfer_to_contract_ chain.originated_contract (Remove 1n) bond in
    let result_remove = Unit.act_as operator remove_block in
    let storage = Test.get_storage chain.originated_typed_address in
    let alice_balance_after_remove = Test.get_balance alice.address in
    Unit.and_list 
    [  sanity_check
    ;  result_remove
    ;  Unit.assert_equals (alice_balance_after_proposal + bond) alice_balance_after_remove "Alice should have been reimbursed"
    ;  Unit.assert_equals 1n (storage.max_index) "max_index should still be 1"
    ;  Unit.assert_equals (None : block option) (get_block (1n, storage)) "the block should have been deleted"
    ]


let _test_remove_parent_of_two () = 
    // setup
    let operator, actors = Unit.init_default () in 
    let alice, bob, _carol = actors in 
    let chain : originated_chain = Unit.act_as operator originate_chain  in
    let bob_balance_at_origination = Test.get_balance bob.address in
    let alice_balance_at_origination = Test.get_balance alice.address in

    let first_block  = 
        {  prototype_block_proposal with
           parent = 0n
        ;  inbox_level = 0n
        ;  hash = 0x0101
        } in 
    let second_block =
        {  prototype_block_proposal with
           parent = 1n
        ;  inbox_level = 10n
        ;  hash = 0x0101
        } in 
    let third_block = 
        {  prototype_block_proposal with
           parent = 1n
        ;  inbox_level = 20n
        ;  hash = 0x0202
        } in 
    let send_block (block : block_proposal) () = Unit.transfer_to_contract_ chain.originated_contract (Receive block) bond in
    let result_first = Unit.act_as alice (send_block first_block) in
    let result_second = Unit.act_as bob (send_block second_block) in
    let result_third = Unit.act_as bob (send_block third_block) in

    // sanity_check
    let storage = Test.get_storage chain.originated_typed_address in
    let bob_balance_after_proposal = Test.get_balance bob.address in
    let alice_balance_after_proposal = Test.get_balance alice.address in
    let sanity_check = Unit.and_list 
    [  Unit.assert_is_ok result_first "first block should have succeeded"
    ;  Unit.assert_is_ok result_second  "second block should have succeeded"
    ;  Unit.assert_is_ok result_third  "third block should have succeeded"
    ;  Unit.assert_equals 3n (storage.max_index) "max_index should be 3"
    ;  Unit.assert_equals (Some [3n ; 2n]) (get_children (1n, storage)) "second block is a child of first"
    ;  Unit.assert_ (alice_balance_after_proposal + bond < alice_balance_at_origination) "Alice should have place bonds"
    ;  Unit.assert_ (bob_balance_after_proposal + bond + bond < bob_balance_at_origination) "Bob should have place 2 bonds"
    ] in

    // act
    let remove_block () = Unit.transfer_to_contract_ chain.originated_contract (Remove 1n) bond in
    let result_remove = Unit.act_as operator remove_block in

    //assert
    let storage = Test.get_storage chain.originated_typed_address in
    let bob_balance_after_remove = Test.get_balance bob.address in
    let alice_balance_after_remove = Test.get_balance alice.address in
    Unit.and_list 
    [  sanity_check
    ;  result_remove
    ;  Unit.assert_equals 3n (storage.max_index) "max_index should still be 3"
    ;  Unit.assert_equals (None : block option) (get_block (1n, storage)) "the block 1 should have been deleted"
    ;  Unit.assert_equals (None : block option) (get_block (2n, storage)) "the block 2 should have been deleted"
    ;  Unit.assert_equals (None : block option) (get_block (3n, storage)) "the block 3 should have been deleted"
    ;  Unit.assert_equals alice_balance_after_remove (alice_balance_after_proposal + bond) "Alice should have been reimbursed"
    ;  Unit.assert_equals bob_balance_after_remove (bob_balance_after_proposal + bond + bond) "Bob should have been reimbursed"
    ]

(* Creation of test suite *)
let suite = Unit.make_suite
    "Chain_sc: receive and remove"
    "Test suite of bridge storage of blocks"
    [  Unit.make_test "First block: success" "test sending first block"  _test_receive_first_block            
    ;  Unit.make_test "Second block: success" "sending a son" _test_receive_son            
    ;  Unit.make_test "Third block: success" "sending two children for first block" _test_receive_siblings                   
    ;  Unit.make_test "Second block: failure" "sending an orphan" _test_receive_orphan                             
    ;  Unit.make_test "Remove: success" "removing a block" _test_remove_block
    ;  Unit.make_test "Remove: success" "removing a parent block" _test_remove_parent_of_two
    ] 