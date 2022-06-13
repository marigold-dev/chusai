#include "../../commons/refutation_interface.mligo"
#import "../src/game.mligo" "Game"
#import "../src/segment.mligo" "Seg"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"

#include "utils.mligo"

(* Tests for apply_choice *)

let make_test_choose (proposer:player) (choice:choice) (state:state) (expected:state) =
    // game is initialised with player1 and player2 (as A et B respectively)
    let game = init_game_ state in
    let new_game = Game.apply_choice (proposer,choice,game) in
    match new_game with 
    | Error _ -> Test_Failed (Message "Should be Some _")
    | Ok g -> Unit.assert_equals (g.state) expected "Game should be finished"


let _test_choose_A_L () =
    make_test_choose (player2 ()) Left (Split (player1 (), (ab 1n, bc 1n))) (End (player1 (),ab 1n))

let _test_choose_A_R () =
    make_test_choose (player2 ()) Right (Split (player1 (), (ab 1n, bc 1n))) (End (player1 (),bc 1n))

let _test_choose_B_L () =
    make_test_choose (player1 ()) Left (Split (player2 (), (ab 1n, bc 1n))) (End (player2 (),ab 1n))

let _test_choose_B_R () =
    make_test_choose (player1 ()) Right (Split (player2 (), (ab 1n, bc 1n))) (End (player2 (),bc 1n))

// let _test_choose_Fail (proposer: unit -> player) (state:unit -> state) (error : Game.Result.error) () = 
//     let game = init_game_ (state ()) in
//     Unit.assert_equals (Game.apply_choice (proposer (),Left,game)) (Error error) "Should fail (be None)"
let assert_fail_with (game : Game.Result.t) (error : Game.Result.error) (msg : string) =
    match game with
    | Ok _ -> Unit.fail_with msg
    | Error e -> if e = error then Unit.succeed () else Unit.fail_with ("Expected "^(Game.Result.error_to_string error)^" but was "^(Game.Result.error_to_string e))

let make_test_choose_Fail (proposer:player) (choice:choice) (state:state) (error : Game.Result.error) = 
    let game = init_game_ state  in
    assert_fail_with (Game.apply_choice (proposer,choice,game)) error "Should fail"

    
let _test_choose_fail_on_start () =
    make_test_choose_Fail (player2 ()) Left (Start (ab 1n)) Wrong_move

let _test_choose_fail_on_end () =
    make_test_choose_Fail (player1 ()) Left ( End (player3 () , ab 1n)) Wrong_move 

let _test_choose_fail_wrong_proposer () =
    make_test_choose_Fail (player1 ()) Left (Split (player1 () , (ab 1n,bc 1n))) Wrong_player

let _test_choose_fail_wrong_size () =
    make_test_choose_Fail (player1 ()) Left (Split (player2 () , (ab 2n,bc 1n)))  Segment_too_long

(* SUITE *)

let suite = Unit.make_suite 
    "Refutation" 
    "test suite for Game lib (apply_choice)"
    [ Unit.make_test
        "Choose on a split: A,Left"
        "Different cases of choose on a split" 
        _test_choose_A_L
    ; Unit.make_test
        "Choose on a split: A,Right"
        "Different cases of choose on a split" 
        _test_choose_A_R
    ; Unit.make_test
        "Choose on a split: B,Left"
        "Different cases of choose on a split" 
        _test_choose_B_L
    ; Unit.make_test
        "Choose on a split: B,Right"
        "Different cases of choose on a split" 
        _test_choose_B_R
    ; Unit.make_test
        "Choose on start: fail"
        "Can't apply choose on state 'Start'"
        _test_choose_fail_on_start
    ; Unit.make_test
        "Choose on end: fail"
        "Can't apply choose on state 'End'"
        _test_choose_fail_on_end
    ; Unit.make_test
        "Choose on end: fail"
        "Can't propose choose on a split you proposed"
        _test_choose_fail_wrong_proposer
    ; Unit.make_test
        "Choose on end: fail"
        "Can't propose choose on a segment longer than 1"
        _test_choose_fail_wrong_size
    ]