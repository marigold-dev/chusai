#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#import "../src/bissection_sc.mligo" "Bissection"
#include "utils.mligo"

(* TESTS *)  

let _test_init () = 
    let operator,actors = Unit.init_default () in
    let alice,bob,carol = actors in
    let file = "refutation/src/bissection_sc.mligo" in
    let views = ["get_game"; "my_games"] in
    let storage = Test.compile_value bissection_default_storage in
    let address, _, _ = Test.originate_from_file file "main" views storage 0tez in
    let game_list_alice : game_id list option = Tezos.call_view "my_games" () address in
    let game_0 : game option = Tezos.call_view "get_game" 0n address in
    Unit.and_list 
    [  Unit.assert_equals (Test.get_balance address) (0tez) "Should be 0"   
    ;  Unit.assert_equals game_list_alice (None : game_id list option) "Should be None"
    ;  Unit.assert_equals game_0 (None : game option) "Should be None"
    ]
    

(* SUITE*)

let suite = 
    Unit.make_suite
    "Refutation sc (views)"
    "test of the smart contract : views"
    [   Unit.make_test
        "Origination"
        "Test only the origination of the contract, including views"
        _test_init 
    ]