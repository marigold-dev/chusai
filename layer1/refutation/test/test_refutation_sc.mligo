#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#import "../src/bissection_sc.mligo" "Bissection"
#include "utils.mligo"

(* TESTS *)    
let _test_originate () = 
    let operator,users = Unit.init_default () in
    let originated_bissection = Unit.act_as operator originate_bissection in
    Unit.assert_equals (Test.get_balance originated_bissection.originated_address) (0tez) "Should be 0"   
    

let _test_start_game () = 
    // init: two players, a game
    let operator,users = Unit.init_default () in
    let alice,bob,carol = users in
    let arbiter = originate_bissection () in

    // run game
    let segment = ac 10n in
    let start_result = 
        Unit.act_as carol
        (fun () -> Unit.transfer_to_contract_ 
            arbiter.originated_contract 
            (Endpoint_Start (segment,alice.address,bob.address)) 
            0tez
        )
    in
    // asserts : a lazy list to make sure that if starting fails, and there is no game, the tests doesn't interrupt the suite
    Unit.and_lazy_list
    [  fun () -> start_result
    ;  fun () -> Unit.assert_equals (get_max_id arbiter) 1n "There should be one game"
    ;  fun () -> let game = get_game arbiter 1n in Unit.assert_not_none game "There should be a game"
    ;  fun () -> let game = Option.unopt (get_game arbiter 1n) in 
        Unit.and_list 
        [  Unit.assert_equals game.state (Start segment) "The initial state should be start"
        ;  Unit.assert_equals game.player_a alice.address "The player that proposed the segment is Alice"
        ;  Unit.assert_equals game.player_b bob.address "The player that should propose first dissection is Bob"
        ]
    ]

let _test_start_dissection () = 
    // init: two players, a game
    let operator,users = Unit.init_default () in
    let alice,bob,carol = users in
    let arbiter = originate_bissection () in

    // run game
    let segment = ac 10n in
    let dissection = ab 5n, bd 5n in
    let start_result = 
        Unit.act_as bob
        (fun () -> Unit.transfer_to_contract_ 
            arbiter.originated_contract
            (Endpoint_Start_Dissection (segment, alice.address, dissection, bob.address))
            0tez
        )
    in
    Unit.and_lazy_list
    [  fun () -> start_result
    ;  fun () -> Unit.assert_equals (get_max_id arbiter) 1n "There should be one game"
    ;  fun () -> let game = get_game arbiter 1n in Unit.assert_not_none game "There should be a game"
    ;  fun () -> let game = Option.unopt (get_game arbiter 1n) in  
        Unit.and_list 
        [  Unit.assert_equals game.state (Dissection (bob.address, dissection)) "The initial state should be start"
        ;  Unit.assert_equals game.player_a alice.address "The player that proposed the segment is Alice"
        ;  Unit.assert_equals game.player_b bob.address "The player that should propose first dissection is Bob"
        ]
    ]
   
let _test_start_wrong_dissection () = 
    // init: two players, a game
    let operator,users = Unit.init_default () in
    let alice,bob,carol = users in
    let arbiter = originate_bissection () in

    // run game
    let segment = ac 10n in
    let dissection = ab 5n, bc 5n in // the dissection should end the same way as initial segment
    let start_result = 
        Unit.act_as bob
        (fun () -> Unit.transfer_to_contract_ 
            arbiter.originated_contract
            (Endpoint_Start_Dissection (segment, alice.address, dissection, bob.address))
            0tez
        )
    in
    Unit.and_lazy_list
    [  fun () -> Unit.assert_rejected start_result "The transactions should have failed : dissection is wrong"
    ;  fun () -> Unit.assert_equals (get_max_id arbiter) 0n "There should be no game"
    ;  fun () -> let game = get_game arbiter 1n in Unit.assert_equals game (None : game option) "There should NOT be a game"
    ] 

(* SUITE*)

let suite = 
    Unit.make_suite
    "Refutation sc (main)"
    "test of the smart contract : endpoints"
    [   Unit.make_test
        "Origination"
        "Test only the origination of the contract"
        _test_originate
    
    ;   Unit.make_test 
        "Start a game"
        "Start a simple game and check starting status"
        _test_start_game
        
    ;   Unit.make_test 
        "Start a game and apply a dissection"
        "Start a simple game and apply a first dissection"
        _test_start_dissection
        
    ;   Unit.make_test 
        "Start a game and apply a wrong dissection"
        "Try to start a simple game and apply a first dissection, but dissection is wrong"
        _test_start_wrong_dissection
    ]