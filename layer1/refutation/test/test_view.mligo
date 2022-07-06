#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#import "../src/bissection_sc.mligo" "Bissection"
#include "utils.mligo"

(* ************************************ *)
(* CONTEXT DEFINITION *)  

type context = 
    {  operator : Unit.actor
    ;  alice : Unit.actor
    ;  bob : Unit.actor
    ;  carol : Unit.actor
    ;  arbiter : originated_arbiter
    }
let init_context () =
    let operator,actors = Unit.init_default () in
    let alice,bob,carol = actors in
    let arbiter = Unit.act_as operator (fun () -> originate_bissection_from_file ()) in
    { operator = operator 
    ; alice = alice
    ; bob = bob
    ; carol = carol
    ; arbiter = arbiter 
    }

(* ************************************ *)
(* VIEW CALLING UTILS*)

let call_view_test (type p res) (name:string) (data:p) (addr:address) : res option =
  let x =
    Test.run
      (fun (addr:address) -> (Tezos.call_view name data addr : res option))
      addr
  in
  (Test.decompile x : res option)

let call_view_test_as (type p res) (actor:Unit.actor) (name:string) (data:p) (addr:address) : res option =
    Unit.act_as actor (fun () -> (call_view_test name data addr : res option))

(* ************************************ *)
(* TESTS my_games *)

let _test_init_my_games () = 
    let ctx = init_context () in

    let game_list_alice : (game_id list) option option = call_view_test_as ctx.alice "my_games" ()  ctx.arbiter.originated_address  in
    Unit.and_list 
    [  Unit.assert_not_none game_list_alice "view calling my_games should have been success (not none)"
    ;  Unit.assert_equals game_list_alice (Some (None : (game_id list) option) : (game_id list) option option) "Should be None"   
    ]
    
let _test_one_game_my_games () = 
    // setup
    let ctx = init_context () in
    // setup: start a game
    let segment = ac 10n in
    let start_result = 
        Unit.act_as ctx.carol
        (fun () -> Unit.transfer_to_contract_ 
            ctx.arbiter.originated_contract 
            (Endpoint_Start (segment,ctx.alice.address,ctx.bob.address)) 
            0tez
        )
    in
    let sanity_check = 
        Unit.and_lazy_list
        [  fun () -> start_result
        (* sanity check: looking directly in the storage*)
        ;  fun () -> Unit.assert_equals (get_max_id ctx.arbiter) 1n "sanity check: There should be one game"
        ;  fun () -> let game = get_game ctx.arbiter 1n in Unit.assert_not_none game "sanity check: There should be a game at index 1"
        ] 
    in

    // act: call the view
    let game_list_alice : (game_id list) option option  = call_view_test_as ctx.alice "my_games" ()  ctx.arbiter.originated_address  in
    let game_list_bob : (game_id list) option option = call_view_test_as ctx.bob "my_games" ()  ctx.arbiter.originated_address  in

    // asserts : a lazy list to make sure that if starting fails, and there is no game, the tests doesn't interrupt the suite
    Unit.and_lazy_list
    [  fun () -> sanity_check
    ;  fun () -> Unit.assert_equals game_list_alice (Some (Some [1n])) "There should be one game for Alice, id 1"
    ;  fun () -> Unit.assert_equals game_list_bob (Some (Some [1n])) "There should be one game for Bob, id 1"
    ]

let _test_two_game_my_games () = 
    // setup
    let ctx = init_context () in
    // setup: start two games
    let segment = ac 10n in
    let start_result1 = 
        Unit.act_as ctx.carol
        (fun () -> Unit.transfer_to_contract_ 
            ctx.arbiter.originated_contract 
            (Endpoint_Start (segment,ctx.alice.address,ctx.bob.address)) 
            0tez
        )
    in
    let start_result2 = 
        Unit.act_as ctx.alice
        (fun () -> Unit.transfer_to_contract_ 
            ctx.arbiter.originated_contract 
            (Endpoint_Start (segment,ctx.bob.address,ctx.carol.address)) 
            0tez
        )
    in
    
    let sanity_check = 
        Unit.and_lazy_list
        [  fun () -> Unit.and start_result1 start_result2
        (* sanity check: looking directly in the storage*)
        ;  fun () -> Unit.assert_equals (get_max_id ctx.arbiter) 2n "sanity check: There should be one game"
        ;  fun () -> let game = get_game ctx.arbiter 1n in Unit.assert_not_none game "sanity check: There should be a game at index 1"
        ;  fun () -> let game = get_game ctx.arbiter 2n in Unit.assert_not_none game "sanity check: There should be a game at index 2"
        ] 
    in

    // act: calling the views
    let game_list_alice : (game_id list) option option  = call_view_test_as ctx.alice "my_games" ()  ctx.arbiter.originated_address  in
    let game_list_bob : (game_id list) option option = call_view_test_as ctx.bob "my_games" ()  ctx.arbiter.originated_address  in
    let game_list_carol : (game_id list) option option = call_view_test_as ctx.carol "my_games" ()  ctx.arbiter.originated_address  in

    // asserts : a lazy list to make sure that if starting fails, and there is no game, the tests doesn't interrupt the suite
    Unit.and_lazy_list
    [  fun () -> sanity_check
    ;  fun () -> Unit.assert_equals game_list_alice (Some (Some [1n])) "There should be one game for Alice, id 1"
    ;  fun () -> Unit.assert_equals game_list_bob (Some (Some [2n;1n])) "There should be two game for Bob, id 1 et 2"
    ;  fun () -> Unit.assert_equals game_list_carol (Some (Some [2n])) "There should be one game for Carol, id 2"
    ]

(* ************************************ *)
(* TESTS get_game *)

let _test_init_get_game () = 
    let ctx = init_context () in
    let game_0 : game option option  = call_view_test_as ctx.operator "get_game" 0n ctx.arbiter.originated_address in
    Unit.and_list 
    [  Unit.assert_not_none game_0 "view calling get_game should have been success (not none)"
    ;  Unit.assert_equals game_0 (Some (None : game option) : game option option) "Should be None"
    ]

let _test_one_game_get_game () = 
    // setup
    let ctx = init_context () in
    // setup: start a game
    let segment = ac 10n in
    let start_result = 
        Unit.act_as ctx.carol
        (fun () -> Unit.transfer_to_contract_ 
            ctx.arbiter.originated_contract 
            (Endpoint_Start (segment,ctx.alice.address,ctx.bob.address)) 
            0tez
        )
    in
    
    let sanity_check = 
        Unit.and_lazy_list
        [  fun () -> start_result
        (* sanity check: looking directly in the storage*)
        ;  fun () -> Unit.assert_equals (get_max_id ctx.arbiter) 1n "sanity check: There should be one game"
        ;  fun () -> let game = get_game ctx.arbiter 1n in Unit.assert_not_none game "sanity check: There should be a game at index 1"
        ] 
    in
    // act: calling the views
    let game_0 : game option option  = call_view_test_as ctx.operator "get_game" 0n ctx.arbiter.originated_address in
    let game_1 : game option option  = call_view_test_as ctx.operator "get_game" 1n ctx.arbiter.originated_address in

    // asserts : a lazy list to make sure that if starting fails, and there is no game, the tests doesn't interrupt the suite
    Unit.and_lazy_list
    [  fun () -> sanity_check
    ;  fun () -> Unit.assert_not_none game_1 "view call has succeeded"
    ;  fun () -> Unit.assert_not_none (Option.unopt game_1) "game with id 1 should exist"
    ;  fun () -> 
        let game = Option.unopt (Option.unopt game_1)
        in 
            Unit.and_list 
            [  Unit.assert_equals game.player_a ctx.alice.address "Alice is player_a"
            ;  Unit.assert_equals game.player_b ctx.bob.address "Bob is player_b"
            ]
    ]

(* SUITE*)

let suite = 
    Unit.make_suite
    "Refutation sc (views)"
    "test of the smart contract : views"
    [   Unit.make_test
        "my_game: origination"
        "Test only the origination of the contract, including views"
        _test_init_my_games 

    ;   Unit.make_test
        "my_games: one game"
        "Test with one started game"
        _test_one_game_my_games

    ;   Unit.make_test
        "my_games: two game"
        "Test with two started game"
        _test_two_game_my_games

    ;   Unit.make_test
        "get_game: origination"
        "Test only the origination of the contract, including views"
        _test_init_get_game 

    ;   Unit.make_test
        "get_game: One game"
        "Test with one started game"
        _test_one_game_get_game
    ]