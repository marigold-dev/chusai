#include "../src/chain_sc.mligo"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"

(* UTILS *)
type originated = Unit.originated // FIXME LIGO
type originated_chain = (chain_parameter, chain_storage) originated

let empty_chain : chain = 
    { max_index = 0n 
    ; batches = (Big_map.empty : (index, batch) big_map)
    ; children = (Big_map.empty : (index, index list) big_map)
    }

let originate_chain () : originated_chain = 
    let empty_storage : chain_storage = 
        { empty_chain with
          max_index = 0n 
        ; batches = (Big_map.empty : (index, batch) big_map)
        ; children = (Big_map.empty : (index, index list) big_map)
        } in
    Unit.originate main empty_storage 0tez

(* TESTS *)


// test sending a block
let _test_receive_first_block () = 
    let operator, actors = Unit.init_default () in 
    let alice, _bob, _carol = actors in 
    let chain : originated_chain = Unit.act_as operator originate_chain  in
    let my_batch  = 
        {  parent = 0n
        ;  level = 0n
        ;  hash = 0x0101
        ;  proposer = alice.address
        } in 
    let send_block () = Unit.transfer_to_contract_ chain.originated_contract (Receive my_batch) 0tez in
    let result = Unit.act_as alice send_block in
    let storage = Test.get_storage chain.originated_typed_address in
    Unit.and_list 
    [  result 
    ;  Unit.assert_equals 1n (storage.max_index) "max_index should be 1"
    ;  Unit.assert_equals (Some my_batch) (get_batch (1n, storage)) "the batch should have been stored"
    ]


// test sending its son
let _test_receive_son () = 
    // setup
    let operator, actors = Unit.init_default () in 
    let alice, bob, _carol = actors in 
    let chain : originated_chain = Unit.act_as operator originate_chain  in

    // act
    let first_batch  = 
        {  parent = 0n
        ;  level = 0n
        ;  hash = 0x0101
        ;  proposer = alice.address
        } in 
    let second_batch = 
        {  parent = 1n
        ;  level = 10n
        ;  hash = 0x0101
        ;  proposer = bob.address
        } in 
    let send_block (batch : batch) () = Unit.transfer_to_contract_ chain.originated_contract (Receive batch) 0tez in
    let result_alice = Unit.act_as alice (send_block first_batch) in
    let result_bob = Unit.act_as bob (send_block second_batch) in

    // assert
    let storage = Test.get_storage chain.originated_typed_address in
    Unit.and_list 
    [  Unit.assert_is_ok result_alice "first batch should have succeeded"
    ;  Unit.assert_is_ok result_bob  "second batch should have succeeded"
    ;  Unit.assert_equals 2n (storage.max_index) "max_index should be 2"
    ;  Unit.assert_equals (Some first_batch) (get_batch (1n, storage)) "the first batch should have been stored"
    ;  Unit.assert_equals (Some second_batch) (get_batch (2n, storage)) "the second batch should have been stored"
    ;  Unit.assert_equals (Some [2n]) (get_children (1n, storage)) "second batch is a child of first"
    ]

// test sending an orphan
let _test_receive_orphan () = 
    // setup
    let operator, actors = Unit.init_default () in 
    let alice, bob, _carol = actors in 
    let chain : originated_chain = Unit.act_as operator originate_chain  in
    
    // act
    let first_batch  = 
        {  parent = 0n
        ;  level = 0n
        ;  hash = 0x0101
        ;  proposer = alice.address
        } in 
    let second_batch = 
        {  parent = 3n
        ;  level = 10n
        ;  hash = 0x0101
        ;  proposer = bob.address
        } in 
    let send_block (batch : batch) () = Unit.transfer_to_contract_ chain.originated_contract (Receive batch) 0tez in
    let result_alice = Unit.act_as alice (send_block first_batch) in
    let result_bob = Unit.act_as bob (send_block second_batch) in

    // assert
    let storage = Test.get_storage chain.originated_typed_address in
    Unit.and_list 
    [  Unit.assert_is_ok result_alice "first batch should have succeeded"
    ;  Unit.assert_rejected result_bob  "second batch should have failed"
    ;  Unit.assert_equals 1n (storage.max_index) "max_index should be 1: the second batch failed"
    ;  Unit.assert_equals (Some first_batch) (get_batch (1n, storage)) "the batch should have been stored"
    ;  Unit.assert_equals (None : index list option) (get_children (1n, storage)) "second batch not recorded as a child of first"
    ]


// test sending its son
let _test_receive_siblings () = 
    // setup
    let operator, actors = Unit.init_default () in 
    let alice, bob, _carol = actors in 
    let chain : originated_chain = Unit.act_as operator originate_chain  in

    // act
    let first_batch  = 
        {  parent = 0n
        ;  level = 0n
        ;  hash = 0x0101
        ;  proposer = alice.address
        } in 
    let second_batch = 
        {  parent = 1n
        ;  level = 10n
        ;  hash = 0x0101
        ;  proposer = bob.address
        } in 
    let third_batch = 
        {  parent = 1n
        ;  level = 20n
        ;  hash = 0x0202
        ;  proposer = alice.address
        } in 
    let send_block (batch : batch) () = Unit.transfer_to_contract_ chain.originated_contract (Receive batch) 0tez in
    let result_first = Unit.act_as alice (send_block first_batch) in
    let result_second = Unit.act_as bob (send_block second_batch) in
    let result_third = Unit.act_as bob (send_block third_batch) in

    // assert
    let storage = Test.get_storage chain.originated_typed_address in
    Unit.and_list 
    [  Unit.assert_is_ok result_first "first batch should have succeeded"
    ;  Unit.assert_is_ok result_second  "second batch should have succeeded"
    ;  Unit.assert_is_ok result_third  "third batch should have succeeded"
    ;  Unit.assert_equals 3n (storage.max_index) "max_index should be 3"
    ;  Unit.assert_equals (Some [3n ; 2n]) (get_children (1n, storage)) "second batch is a child of first"
    ]


(* Creation of test suite *)
let suite = Unit.make_suite
"Chain_sc"
"Test suite of bridge storage of batches"
[  Unit.make_test "First block: success" "test sending first block"  _test_receive_first_block           
;  Unit.make_test "Second block: success" "sending a son" _test_receive_son            
;  Unit.make_test "Third block: success" "sending two children for first block" _test_receive_siblings                   
;  Unit.make_test "Second block: failure" "sending an orphan" _test_receive_orphan           
]