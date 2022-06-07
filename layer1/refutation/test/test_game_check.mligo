#include "../../commons/refutation_interface.mligo"
#import "../src/game.mligo" "Game"
#import "../src/segment.mligo" "Seg"
#import "../../stdlib_ext/src/atomic_test.mligo" "Atom"
#include "utils.mligo"

(* Tests for check_player *)

let _test_check_player () =
    let game = init_game () in
    Atom.and_list 
    [ Atom.assert_equals (Game.check_player (player1 (),game)) false "A defends, so not first player to play"
    ; Atom.assert_equals (Game.check_player (player2 (),game)) true "B has to provide a split"
    ; Atom.assert_equals (Game.check_player (player3 (),game)) false "Not part of the game"
    ]

let _test_check_player_asplit () =
    let game = init_game_ (Split (player1 (), (ab 5n, bc 5n))) in
    Atom.and_list 
    [ Atom.assert_equals (Game.check_player (player1 (),game)) false "A was last top play"
    ; Atom.assert_equals (Game.check_player (player2 (),game)) true "B has to provide a split"
    ; Atom.assert_equals (Game.check_player (player3 (),game)) false "Not part of the game"
    ]



(* SUITE *)

let suite = Atom.make_suite 
    "Refutation: test suite for Game lib (check)"
    [ Atom.make_test 
        "Check player: start"
        "test for different players against game in starting state" 
        _test_check_player
    ; Atom.make_test
        "Check player: Asplit"
        "test for different players against game in state Asplit" 
        _test_check_player_asplit
    ]