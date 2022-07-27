#include "../test/chain_sc.mligo"
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
let _test_withdraw () = 
    let operator, actors = Unit.init_default_at ("2020-01-01t10:10:10Z" : timestamp) in 
    let alice, _bob, _carol = actors in 
    let asset : frozen_amount = 10000000n in // should be high enough to compensate for gas
    let init_chain = 
        { empty_chain with
          max_index = 1n 
        ; freezer = Big_map.literal [(alice.address, asset)]
        } in
    // sanity check
    let assets_opt, new_chain = withdraw (alice.address, init_chain) in
    let sanity_check = Unit.and_list 
    [  Unit.assert_equals (Some asset) assets_opt "Alice should have some assets to withdraw"
    ;  Unit.assert_equals (None : frozen_amount option) (Big_map.find_opt alice.address new_chain.freezer) "There should be no more asset to withdraw"
    ] in

    // origination
    let chain : originated_chain = Unit.act_as operator (originate_chain_with init_chain (asset * 1mutez))  in
    let alice_initial_balance = Test.get_balance alice.address in

    // act
    let finalize_result = Unit.act_as alice (fun () -> Unit.transfer_to_contract_ chain.originated_contract Withdraw 0tez) in

    //assert
    let storage = Test.get_storage chain.originated_typed_address in
    let alice_new_balance = Test.get_balance alice.address in
    Unit.and_list 
    [  sanity_check
    ;  Unit.assert_is_ok finalize_result "withdrawal should have succeeded" 
    ;  Unit.assert_ (alice_initial_balance < alice_new_balance) "Alice should have received the funds"
    ]


(* Creation of test suite *)
let suite = Unit.make_suite
"Chain_sc : withdraw"
"Test suite of withdrawal"
[  Unit.make_test "simple withdrawal" "test straightforward withdraw"  _test_withdraw               
]
