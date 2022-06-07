#include "../../commons/refutation_interface.mligo"
#import "../src/game.mligo" "Game"
#import "../src/segment.mligo" "Seg"
#import "../../stdlib_ext/src/atomic_test.mligo" "Atom"

#include "utils.mligo"


(* Tests for apply_choice *)

let make_test_choose (proposer:player) (choice:choice) (state:state) (expected:state) =
    // game is initialised with player1 and player2 (as A et B respectively)
    let game = init_game_ state in
    let new_game = Game.apply_choice (proposer,choice,game) in
    match new_game with 
    | None -> Status_Fail (Message "Should be Some _")
    | Some g -> Atom.assert_equals (g.state) expected "Game should be finished"


let _test_choose_A_L () =
    make_test_choose (player2 ()) Left (Split (player1 (), (ab 1n, bc 1n))) (End (player1 (),ab 1n))

let _test_choose_A_R () =
    make_test_choose (player2 ()) Right (Split (player1 (), (ab 1n, bc 1n))) (End (player1 (),bc 1n))

let _test_choose_B_L () =
    make_test_choose (player1 ()) Left (Split (player2 (), (ab 1n, bc 1n))) (End (player2 (),ab 1n))

let _test_choose_B_R () =
    make_test_choose (player1 ()) Right (Split (player2 (), (ab 1n, bc 1n))) (End (player2 (),bc 1n))

let _test_choose_Fail (proposer: unit -> player) (state:unit -> state) () = 
    let game = init_game_ (state ()) in
    Atom.assert_equals (Game.apply_choice (proposer (),Left,game)) (None:game option) "Should fail (be None)"


let make_test_choose_Fail (proposer:player) (choice:choice)  (state:state)  = 
    let game = init_game_ state  in
    Atom.assert_equals (Game.apply_choice (proposer,choice,game)) (None:game option) "Should fail (be None)"

    
let _test_choose_fail_on_start () =
    make_test_choose_Fail (player1 ()) Left (Start (ab 1n))

let _test_choose_fail_on_end () =
    make_test_choose_Fail (player1 ()) Left ( End (player3 () , ab 1n))

let _test_choose_fail_wrong_proposer () =
    make_test_choose_Fail (player1 ()) Left (Split (player1 () , (ab 1n,bc 1n)))

let _test_choose_fail_wrong_size () =
    make_test_choose_Fail (player1 ()) Left (Split (player2 () , (ab 2n,bc 1n)))

(* SUITE *)

let suite = Atom.make_suite 
    "Refutation: test suite for Game lib (apply_choice)"
    [ Atom.make_test
        "Choose on a split: A,Left"
        "Different cases of choose on a split" 
        _test_choose_A_L
    ]