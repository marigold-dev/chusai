#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#import "../src/bissection_sc.mligo" "Bissection"
#include "utils.mligo"

(* TESTS *)  

let call_view_test (type p res) (name:string) (data:p) (addr:address) : res option =
  let x =
    Test.run
      (fun (addr:address) -> (Tezos.call_view name data addr : res option))
      addr
  in
  (Test.decompile x : res option)

let _test_init () = 
    let operator,actors = Unit.init_default () in
    let alice,bob,carol = actors in
    let file = "refutation/src/bissection_sc.mligo" in
    let views = ["get_game"; "my_games"] in
    let storage = Test.compile_value bissection_default_storage in
    let arbiter = originate_bissection_from_file () in
    let game_list_alice : game_id list option = call_view_test "my_games" ()  arbiter.originated_address in
    let game_0 : game option = call_view_test "get_game" 0n arbiter.originated_address in
    Unit.and_list 
    [  Unit.assert_equals (Test.get_balance arbiter.originated_address) (0tez) "Should be 0"   
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