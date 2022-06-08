#include "../../commons/refutation_interface.mligo"
#import "../src/game.mligo" "Game"
#import "../src/segment.mligo" "Seg"
#import "../../stdlib_ext/src/atomic_test.mligo" "Atom"

#include "utils.mligo"

let assert_not_none (type a ) (val : a option) (default : a) (msg : string) =
    match val with
    | None -> Atom.fail msg , default
    | Some v -> Atom.succeed () , v

let _test_split () =
    let game = init_game_ (Start (ac 10n)) in
    let split = (ab 5n), (bd 5n) in
    let opt = Game.apply_split  (player2 (), split, Left, game ) in
    let status,new_game = assert_not_none opt (init_game ()) "Should have applied split successfully" in
    Atom.and_list 
    [  status
    ; Atom.assert_equals (new_game.state) (Split (player2 (), split)) "Should be a split"
    ]

(* SUITE *)

let suite = Atom.make_suite 
    "Refutation: test suite for Game lib (apply_split)"
    [ Atom.make_test
        "Split new game"
        "Apply a split on a new game" 
        _test_split
    ]