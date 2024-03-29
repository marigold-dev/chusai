#include "../../commons/refutation_interface.mligo"
#import "../src/segment.mligo" "Seg"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#include "utils.mligo"

let _test_choose_left () = 
    let s1 = ab 2n in
    let s2 = bc 5n in
    let dissection = s1,s2 in
    Unit.assert_equals (Seg.choose (Left,dissection)) s1 "Should be s1"

let _test_choose_right () = 
    let s1 = ab 2n in
    let s2 = bc 5n in
    let dissection = s1,s2 in
    Unit.assert_equals (Seg.choose (Right,dissection)) s2 "Should be s2" 

let string_of_bool (b:bool) = 
    if b then "true" else "false"

let _test_check_dissection_against_segment (dissection,segment : dissection * segment) (expected:bool) () =
    Unit.assert_equals (Seg.check_dissection_against_segment (dissection ,segment)) expected ("Should be "^(string_of_bool expected))
    

let suite = 
    Unit.make_suite
    "Refutation"
    "test suite for Segment module of refutation game"
    [ Unit.make_test "Choose left" "choose Left (s1,s2)" _test_choose_left
    ; Unit.make_test "Choose right" "choose Right (s1,s2)" _test_choose_right
    ; Unit.make_test 
        "Check dissection against segment" 
        "Normal case OK" 
        (_test_check_dissection_against_segment ((ab 5n,bc 5n), ad 10n) true) 
    ; Unit.make_test 
        "Check dissection : wrong end" 
        "End of the dissection should be different" 
        (_test_check_dissection_against_segment ((ab 5n,bc 5n), ac 10n) false)  
    ; Unit.make_test 
        "Check dissection : wrong start" 
        "start of dissection is not start of segment" 
        (_test_check_dissection_against_segment ((ab 5n,bc 5n), bd 10n) false)
    ; Unit.make_test 
        "Check dissection : not consecutive" 
        "The dissection should be two consecutive segments" 
        (_test_check_dissection_against_segment ((ac 5n,bd 5n), ac 10n) false)
    ; Unit.make_test 
        "Check dissection : wrong size" 
        "size of dissection should be equals to size of segment" 
        (_test_check_dissection_against_segment ((ab 5n,bc 5n), ad 8n) false)
    ; Unit.make_test 
        "Check dissection : wrong size" 
        "size of dissection should be equals to size of segment" 
        (_test_check_dissection_against_segment ((ab 5n,bc 5n), ad 12n) false)
    ]