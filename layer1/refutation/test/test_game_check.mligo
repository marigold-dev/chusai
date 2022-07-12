#include "../../commons/refutation_interface.mligo"
#import "../src/game.mligo" "Game"
#import "../src/segment.mligo" "Seg"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#include "utils.mligo"

(* Tests for check_player *)

let _test_check_player () =
    let game = init_game () in
    Unit.and_list 
    [ Unit.assert_equals (Game.check_player (player1 (),game)) false "A defends, so not first player to play"
    ; Unit.assert_equals (Game.check_player (player2 (),game)) true "B has to provide a dissection"
    ; Unit.assert_equals (Game.check_player (player3 (),game)) false "Not part of the game"
    ]

let _test_check_player_adissection () =
    let game = init_game_ (Dissection (player1 (), (ab 5n, bc 5n))) in
    Unit.and_list 
    [ Unit.assert_equals (Game.check_player (player1 (),game)) false "A was last to play"
    ; Unit.assert_equals (Game.check_player (player2 (),game)) true "B has to provide a dissection"
    ; Unit.assert_equals (Game.check_player (player3 (),game)) false "Not part of the game"
    ]


let _test_check_player_bdissection () =
    let game = init_game_ (Dissection (player2 (), (ab 5n, bc 5n))) in
    Unit.and_list 
    [ Unit.assert_equals (Game.check_player (player1 (),game)) true "A should provide dissection"
    ; Unit.assert_equals (Game.check_player (player2 (),game)) false "B was last to play"
    ; Unit.assert_equals (Game.check_player (player3 (),game)) false "Not part of the game"
    ]

let _test_check_player_end () =
    let game = init_game_ (End (player2 (), ab 5n)) in
    Unit.and_list 
    [ Unit.assert_equals (Game.check_player (player1 (),game)) false "game has ended"
    ; Unit.assert_equals (Game.check_player (player2 (),game)) false "game has ended"
    ; Unit.assert_equals (Game.check_player (player3 (),game)) false "Not part of the game"
    ]
(* SUITE *)

let suite = Unit.make_suite 
    "Refutation - check" 
    "test suite for Game lib (check)"    
    [ Unit.make_test 
        "Check player: start"
        "test for different players against game in starting state" 
        _test_check_player
    ; Unit.make_test
        "Check player: Adissection"
        "test for different players against game in state dissection" 
        _test_check_player_adissection
    ; Unit.make_test
        "Check player: Bdissection"
        "test for different players against game in state dissection" 
        _test_check_player_bdissection
    ; Unit.make_test
        "Check player: End"
        "test for different players against game in state end" 
        _test_check_player_end
    ]