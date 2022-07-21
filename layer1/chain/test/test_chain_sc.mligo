#include "../src/chain_sc.mligo"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"

(* UTILS *)
type originated = Unit.originated // FIXME LIGO
type originated_chain = (chain_parameter, chain_storage) originated

let empty_chain : chain = 
    { max_index = 0n 
    ; blocks = (Big_map.empty : (index, block) big_map)
    ; children = (Big_map.empty : (index, index list) big_map)
    }

let originate_chain () : originated_chain = 
    let empty_storage : chain_storage = 
        { empty_chain with
          max_index = 0n 
        ; blocks = (Big_map.empty : (index, block) big_map)
        ; children = (Big_map.empty : (index, index list) big_map)
        } in
    Unit.originate main empty_storage 0tez

(* TESTS *)


// test sending a block
let _test_receive_first_block () = 
    let operator, actors = Unit.init_default () in 
    let alice, _bob, _carol = actors in 
    let chain : originated_chain = Unit.act_as operator originate_chain  in
    let my_block  = 
        {  parent = 0n
        ;  level = 0n
        ;  hash = 0x0101
        ;  proposer = alice.address
        } in 
    let send_block () = Unit.transfer_to_contract_ chain.originated_contract (Receive my_block) 0tez in
    let result = Unit.act_as alice send_block in
    let storage = Test.get_storage chain.originated_typed_address in
    Unit.and_list 
    [  result 
    ;  Unit.assert_equals 1n (storage.max_index) "max_index should be 1"
    ;  Unit.assert_equals (Some my_block) (get_block (1n, storage)) "the block should have been stored"
    ]


// test sending its son
let _test_receive_son () = 
    // setup
    let operator, actors = Unit.init_default () in 
    let alice, bob, _carol = actors in 
    let chain : originated_chain = Unit.act_as operator originate_chain  in

    // act
    let first_block  = 
        {  parent = 0n
        ;  level = 0n
        ;  hash = 0x0101
        ;  proposer = alice.address
        } in 
    let second_block = 
        {  parent = 1n
        ;  level = 10n
        ;  hash = 0x0101
        ;  proposer = bob.address
        } in 
    let send_block (block : block) () = Unit.transfer_to_contract_ chain.originated_contract (Receive block) 0tez in
    let result_alice = Unit.act_as alice (send_block first_block) in
    let result_bob = Unit.act_as bob (send_block second_block) in

    // assert
    let storage = Test.get_storage chain.originated_typed_address in
    Unit.and_list 
    [  Unit.assert_is_ok result_alice "first block should have succeeded"
    ;  Unit.assert_is_ok result_bob  "second block should have succeeded"
    ;  Unit.assert_equals 2n (storage.max_index) "max_index should be 2"
    ;  Unit.assert_equals (Some first_block) (get_block (1n, storage)) "the first block should have been stored"
    ;  Unit.assert_equals (Some second_block) (get_block (2n, storage)) "the second block should have been stored"
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
        {  parent = 0n
        ;  level = 0n
        ;  hash = 0x0101
        ;  proposer = alice.address
        } in 
    let second_block = 
        {  parent = 3n
        ;  level = 10n
        ;  hash = 0x0101
        ;  proposer = bob.address
        } in 
    let send_block (block : block) () = Unit.transfer_to_contract_ chain.originated_contract (Receive block) 0tez in
    let result_alice = Unit.act_as alice (send_block first_block) in
    let result_bob = Unit.act_as bob (send_block second_block) in

    // assert
    let storage = Test.get_storage chain.originated_typed_address in
    Unit.and_list 
    [  Unit.assert_is_ok result_alice "first block should have succeeded"
    ;  Unit.assert_rejected result_bob  "second block should have failed"
    ;  Unit.assert_equals 1n (storage.max_index) "max_index should be 1: the second block failed"
    ;  Unit.assert_equals (Some first_block) (get_block (1n, storage)) "the block should have been stored"
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
        {  parent = 0n
        ;  level = 0n
        ;  hash = 0x0101
        ;  proposer = alice.address
        } in 
    let second_block = 
        {  parent = 1n
        ;  level = 10n
        ;  hash = 0x0101
        ;  proposer = bob.address
        } in 
    let third_block = 
        {  parent = 1n
        ;  level = 20n
        ;  hash = 0x0202
        ;  proposer = alice.address
        } in 
    let send_block (block : block) () = Unit.transfer_to_contract_ chain.originated_contract (Receive block) 0tez in
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


(* Creation of test suite *)
let suite = Unit.make_suite
"Chain_sc"
"Test suite of bridge storage of blocks"
[  Unit.make_test "First block: success" "test sending first block"  _test_receive_first_block           
;  Unit.make_test "Second block: success" "sending a son" _test_receive_son            
;  Unit.make_test "Third block: success" "sending two children for first block" _test_receive_siblings                   
;  Unit.make_test "Second block: failure" "sending an orphan" _test_receive_orphan           
]