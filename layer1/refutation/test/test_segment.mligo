#include "../../commons/refutation_interface.mligo"
#import "../src/segment.mligo" "Seg"
#import "../../stdlib_ext/src/atomic_test.mligo" "Atom"

let _test_choose_left () = 
    let s1 = Seg.make_segment 0n 1n 2n in
    let s2 = Seg.make_segment 3n 4n 5n in
    let split = s1,s2 in
    Atom.assert_equals (Seg.choose (Left,split)) s1 "Should be s1"

let _test_choose_right () = 
    let s1 = Seg.make_segment 0n 1n 2n in
    let s2 = Seg.make_segment 3n 4n 5n in
    let split = s1,s2 in
    Atom.assert_equals (Seg.choose (Right,split)) s2 "Should be s2" 

let string_of_bool (b:bool) = 
    if b then "true" else "false"

let _test_check_split_against_segment (split,segment : split * segment) (expected:bool) () =
    Atom.assert_equals (Seg.check_split_against_segment (split ,segment)) expected ("Should be "^(string_of_bool expected))
    

let suite = 
    let a = 0n in
    let b = 1n in
    let c = 3n in
    let d = 4n in
    let ac (size:size) = Seg.make_segment a c size in
    let ab (size:size) = Seg.make_segment a b size in
    let ad (size:size) = Seg.make_segment a d size in
    let bc (size:size) = Seg.make_segment b c size in
    let bd (size:size) = Seg.make_segment b d size in
    Atom.make_suite
    "Test suite for Segment module of refutation game"
    [ Atom.make_test "Choose left" "choose Left (s1,s2)" _test_choose_left
    ; Atom.make_test "Choose right" "choose Right (s1,s2)" _test_choose_right
    ; Atom.make_test 
        "Check split against segment" 
        "Normal case OK" 
        (_test_check_split_against_segment ((ab 5n,bc 5n), ad 10n) true) 
    ; Atom.make_test 
        "Check split : wrong end" 
        "End of the split should be different" 
        (_test_check_split_against_segment ((ab 5n,bc 5n), ac 10n) false)  
    ; Atom.make_test 
        "Check split : wrong start" 
        "start of split is not start of segment" 
        (_test_check_split_against_segment ((ab 5n,bc 5n), bd 10n) false)
    ; Atom.make_test 
        "Check split : not consecutive" 
        "The split should be two consecutive segments" 
        (_test_check_split_against_segment ((ac 5n,bd 5n), ac 10n) false)
    ; Atom.make_test 
        "Check split : wrong size" 
        "size of split should be equals to size of segment" 
        (_test_check_split_against_segment ((ab 5n,bc 5n), ad 8n) false)
    ; Atom.make_test 
        "Check split : wrong size" 
        "size of split should be equals to size of segment" 
        (_test_check_split_against_segment ((ab 5n,bc 5n), ad 12n) false)
    ]