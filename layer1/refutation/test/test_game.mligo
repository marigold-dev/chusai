#include "../../commons/refutation_interface.mligo"
#import "../src/game.mligo" "Game"
#import "../src/segment.mligo" "Seg"
#import "../../stdlib_ext/src/atomic_test.mligo" "Atom"

let a = 0n
let b = 1n
let c = 3n
let d = 4n
let ac (size:size) = Seg.make_segment a c size
let ab (size:size) = Seg.make_segment a b size
let ad (size:size) = Seg.make_segment a d size
let bc (size:size) = Seg.make_segment b c size
let bd (size:size) = Seg.make_segment b d size

let player1 () = Test.nth_bootstrap_account 0 
let player2 () = Test.nth_bootstrap_account 1
let player3 () = Test.nth_bootstrap_account 2 

let init_game () = 
    let _ = Test.reset_state 5n ([]:tez list) in
    Game.make_game (ac 5n,player1 (),player2 ()) 

let init_game_ (new_state:state) = 
    let game = init_game () in 
    {game with state=new_state}

(* Tests for check_player *)

let _test_check_player () =
    let game = init_game () in
    Atom.and_list 
    [ Atom.assert_equals (Game.check_player (player1 (),game)) false "A defends, so not first player to play"
    ; Atom.assert_equals (Game.check_player (player2 (),game)) true "B has to provide a split"
    ; Atom.assert_equals (Game.check_player (player3 (),game)) false "Not part of the game"
    ]

let _test_check_player_asplit () =
    let game = init_game_ (Asplit (ab 5n, bc 5n)) in
    Atom.and_list 
    [ Atom.assert_equals (Game.check_player (player1 (),game)) false "A was last top play"
    ; Atom.assert_equals (Game.check_player (player2 (),game)) true "B has to provide a split"
    ; Atom.assert_equals (Game.check_player (player3 (),game)) false "Not part of the game"
    ]

let _test_check_player_bsplit () =
    let game = init_game_ (Bsplit (ab 5n, bc 5n)) in
    Atom.and_list 
    [ Atom.assert_equals (Game.check_player (player1 (),game)) true "A has to make a move"
    ; Atom.assert_equals (Game.check_player (player2 (),game)) false "B was last to play"
    ; Atom.assert_equals (Game.check_player (player3 (),game)) false "Not part of the game"
    ]

(* Tests for apply_choice *)

let make_test_choose (choice:choice) (state:state) (expected:state) =
    // game is initialised with player1 and player2 (as A et B respectively)
    let game = init_game_ state in
    let new_game = Game.apply_choice (choice,game) in
    match new_game with 
    | None -> Status_Fail (Message "Should be Some _")
    | Some g -> Atom.assert_equals (g.state) expected "Game should be finished"


let _test_choose_A_L () =
    make_test_choose Left (Asplit (ab 5n, bc 5n)) (End (player1 (),ab 5n))

let _test_choose_A_R () =
    make_test_choose Right (Asplit (ab 5n, bc 5n)) (End (player1 (),bc 5n))

let _test_choose_B_L () =
    make_test_choose Left (Bsplit (ab 5n, bc 5n)) (End (player2 (),ab 5n))

let _test_choose_B_R () =
    make_test_choose Right (Bsplit (ab 5n, bc 5n)) (End (player2 (),bc 5n))

let _test_choose_Fail (state:unit -> state) () = 
    let game = init_game_ (state ()) in
    Atom.assert_equals (Game.apply_choice (Left,game)) (None:game option) "Should fail (be None)"

(* Tests for apply_split*)

(* SUITE *)

let suite = Atom.make_suite 
    "Refutation: test suite for Game lib"
    [ Atom.make_test 
        "Check player: start"
        "test for different players against game in starting state" 
        _test_check_player
    ; Atom.make_test
        "Check player: Asplit"
        "test for different players against game in state Asplit" 
        _test_check_player_asplit
    ; Atom.make_test
        "Check player: Bsplit"
        "test for different players against game in state Bsplit" 
        _test_check_player_bsplit
    // Test for "apply_choice"    
    ; Atom.make_test
        "Choose on a split: A,Left"
        "Different cases of choose on a split" 
        _test_choose_A_L
    ; Atom.make_test
        "Choose on a split: A,Right"
        "Different cases of choose on a split" 
        _test_choose_A_R
    ; Atom.make_test
        "Choose on a split: B,Left"
        "Different cases of choose on a split" 
        _test_choose_B_L
    ; Atom.make_test
        "Choose on a split: B,Right"
        "Different cases of choose on a split" 
        _test_choose_B_R
    ; Atom.make_test
        "Choose on start: fail"
        "Can't apply choose on state 'Start'"
        (_test_choose_Fail (fun () -> Start (ab 5n)))
    ; Atom.make_test
        "Choose on end: fail"
        "Can't apply choose on state 'End'"
        (_test_choose_Fail (fun () -> End (player3 () , ab 5n)))
    // Test for apply_split

    ]