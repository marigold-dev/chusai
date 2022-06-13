#include "../../commons/refutation_interface.mligo"
#import "../src/game.mligo" "Game"
#import "../src/segment.mligo" "Seg"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"

#include "utils.mligo"

let _test_split_ok (state : state) (split : split) (choice : choice option) (proposer : player)  =
    let game = init_game_ state in
    let opt = Game.apply_split  (proposer, split, choice, game ) in
    Unit.and_lazy 
        (Unit.assert_ (Game.Result.is_ok opt) "should not be none" ) 
        (fun () -> 
            let new_game = Game.Result.get_ok opt in 
            Unit.assert_equals  new_game.state (Split (proposer, split)) "Should be a split"
        ) 
    
let _new_game () =
    _test_split_ok 
        (Start (ac 10n)) 
        ((ab 5n), (bd 5n)) 
        (None : choice option)
        (player2 ()) 

let _split_game () =
    _test_split_ok 
        (Split (player1 (), (ac 10n, cd 5n)))
        ((ab 5n), (bd 5n)) 
        (Some Left : choice option)
        (player2 ()) 

let _test_split_ko (state : state) (split : split) (choice : choice option) (proposer : player)  =
    let game = init_game_ state in
    let opt = Game.apply_split  (proposer, split, choice, game ) in
    Unit.assert_ (Game.Result.has_failed opt)  "Should have refused"

let _new_game_wrong_proposer () =
    _test_split_ko 
        (Start (ac 10n)) 
        ((ab 5n), (bd 5n)) 
        (None : choice option)
        (player1 ()) 
    

let _new_game_wrong_proposer_out () =
    _test_split_ko 
        (Start (ac 10n)) 
        ((ab 5n), (bd 5n)) 
        (None : choice option)
        (player3 ()) 


let _new_game_wrong_start () =
    _test_split_ko 
        (Start (ac 10n)) 
        ((bc 5n), (bd 5n)) 
        (None : choice option)
        (player2 ()) 
        
let _new_game_wrong_end () =
    _test_split_ko 
        (Start (ac 10n)) 
        ((ab 5n), (bc 5n)) 
        (None : choice option)
        (player2 ()) 

let _new_game_choice () =
    _test_split_ko
        (Start (ac 10n)) 
        ((ab 5n), (bd 5n)) 
        (Some Left)
        (player2 ()) 

(* SUITE *)

let suite = Unit.make_suite 
    "Refutation"
    "test suite for Game lib (apply_split)"
    [ Unit.make_test
        "Split new game"
        "Apply a split on a new game" 
        _new_game
    ; Unit.make_test
        "Split running game"
        "Apply a split on running game (state is a split)" 
        _split_game
    ; Unit.make_test
        "Split new game : Fails"
        "Apply a split on a new game with wrong proposer" 
        _new_game_wrong_proposer
    ; Unit.make_test
        "Split new game : Fails"
        "Apply a split on a new game with proposer not part of the game" 
        _new_game_wrong_proposer_out
    ; Unit.make_test
        "Split new game : Fails"
        "Apply a split on a new game with non matching start" 
        _new_game_wrong_start
    ; Unit.make_test
        "Split new game : Fails"
        "Apply a split on a new game with matching end" 
        _new_game_wrong_end
    ; Unit.make_test
        "Split new game : Fails"
        "Apply a split on a new game while providing a choice"         
        _new_game_choice
    ]