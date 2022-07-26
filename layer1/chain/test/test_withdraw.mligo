#include "../src/chain.mligo"
#include "utils.mligo"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#import "../../stdlib_ext/src/result.mligo" "Stdlib_Result"

let _test_set_aside_none () = 
    let alice : user = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) in
    let chain = empty_chain in
    let block = { prototype_block with
        cooler = Map.literal [(alice, 42n)]
    } in
    let new_chain = set_aside_frozen_assets (block,chain) in
    Unit.assert_equals (Some 42n) (Big_map.find_opt alice new_chain.freezer) "Alice should have some assets available"

let _test_set_aside_some () = 
    let alice : user = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) in
    let chain = {empty_chain with 
        freezer = Big_map.literal [(alice, 100n)]
    } in
    let block = { prototype_block with
        cooler = Map.literal [(alice, 42n)]
    } in
    let new_chain = set_aside_frozen_assets (block,chain) in
    Unit.assert_equals (Some 142n) (Big_map.find_opt alice new_chain.freezer) "Alice should have more assets available"


let _test_set_aside_multiple () = 
    let alice : user = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) in
    let bob : user = ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) in
    let chain = {empty_chain with 
        freezer = Big_map.literal [(alice, 100n) ; (bob, 1000n)]
    } in
    let block = { prototype_block with
        cooler = Map.literal [(alice, 42n) ; (bob, 24n)]
    } in
    let new_chain = set_aside_frozen_assets (block,chain) in
    Unit.and_list
    [  Unit.assert_equals (Some 142n) (Big_map.find_opt alice new_chain.freezer) "Alice should have more assets available"
    ;  Unit.assert_equals (Some 1024n) (Big_map.find_opt bob new_chain.freezer) "Bob should have more assets available"
    ]

let _test_withdraw_some () =
    let alice : user = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) in
    let chain = {empty_chain with 
        freezer = Big_map.literal [(alice, 100n)]
    } in
    let assets_opt, new_chain = withdraw (alice, chain) in
    Unit.and_list 
    [  Unit.assert_equals (Some 100n) assets_opt "Alice should have some assets to withdraw"
    ;  Unit.assert_equals (None : frozen_amount option) (Big_map.find_opt alice new_chain.freezer) "There should be no more asset to withdraw"
    ]

let _test_withdraw_none () =
    let alice : user = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) in
    let chain = empty_chain  in
    let assets_opt, new_chain = withdraw (alice, chain) in
    Unit.and_list 
    [  Unit.assert_equals (None : frozen_amount option) assets_opt "Alice should have no assets to withdraw"
    ;  Unit.assert_equals (None : frozen_amount option) (Big_map.find_opt alice new_chain.freezer) "There should be no asset to withdraw"
    ]

(* Creation of test suite *)
let suite = Unit.make_suite
"Chain: withdraw"
"Test suite of block storage lib"
[  Unit.make_test "set aside: none before" "set aside frozen asset for a user with no asset frozen"  _test_set_aside_none
;  Unit.make_test "set aside: some before" "set aside frozen asset for a user that already have some assets available"  _test_set_aside_some
;  Unit.make_test "set aside: multiple users" "set aside frozen asset for multiple users that already have some assets available"  _test_set_aside_multiple
;  Unit.make_test "withdraw: some" "Withdraw asset from the freezer"  _test_withdraw_some
;  Unit.make_test "withdraw: none" "Try to withdraw assets when there are none"  _test_withdraw_none
]