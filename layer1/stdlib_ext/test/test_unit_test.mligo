#import "../src/unit_test.mligo" "Unit"

let _test_and_S_S () = 
    let b = Unit.and (Test_Passed 1n) (Test_Passed 2n) = Test_Passed 3n in
    Unit.assert_ b "Should have been success"

let _test_and_S_F () = 
    let b = Unit.and (Test_Passed 1n) (Test_Failed  (Message "shown")) = Test_Failed (Message "shown") in
    Unit.assert_ b "Should have been fail 'shown'"

let _test_and_F_S () = 
    let b = Unit.and (Test_Failed (Message "shown")) (Test_Passed 1n) = Test_Failed (Message "shown") in
    Unit.assert_ b "Should have been fail 'shown'"

let _test_and_F_F () = 
    let b = Unit.and (Test_Failed (Message "shown")) (Test_Failed (Message "hidden")) = Test_Failed (Message "shown") in
    Unit.assert_ b "Should have been fail 'shown'"

let _test_and_S_l () = 
    let b = Unit.and_list 
    [ Test_Passed 1n
    ; Test_Passed 2n
    ; Test_Passed 3n
    ; Test_Passed 5n
    ] = Test_Passed 11n in
    Unit.assert_ b "Should have been fail 'shown'"

let _test_and_S_F_l () = 
    let b = Unit.and_list 
    [ Test_Passed 1n
    ; Test_Failed (Message "shown") 
    ; Test_Failed (Message "hidden")
    ; Test_Passed 5n
    ] = Test_Failed (Message "shown") in
    Unit.assert_ b "Should have been fail 'shown'"

(* run allows partial evaluation : if the last known status is "fail" then subsequent expressions are not evaluated*)
let _test_run_F_failwith () = 
    let to_run = (fun () -> (begin failwith ""; Test_Failed (Message "hidden") end)) in
    let b = Unit.and_lazy (Test_Failed (Message "shown")) to_run = Test_Failed (Message "shown") in
    Unit.assert_ b "Should have been fail 'shown'"

let _test_run_F_F () = 
    let to_run = (fun () -> Test_Failed (Message "hidden") ) in
    let b = Unit.and_lazy (Test_Failed (Message "shown")) to_run = Test_Failed (Message "shown") in
    Unit.assert_ b "Should have been fail 'shown'"
    
let _test_run_S_F () = 
    let to_run = (fun () -> Test_Failed (Message "shown") ) in 
    let b = Unit.and_lazy (Test_Passed 1n) to_run = Test_Failed (Message "shown") in
    Unit.assert_ b "Should have been fail 'shown'"

    
let _test_run_S_S () = 
    let to_run = (fun () -> Test_Passed 2n ) in
    let b = Unit.and_lazy (Test_Passed 1n) to_run = Test_Passed 3n in
    Unit.assert_ b "Should have been Success 3n"


let suite = Unit.make_suite
    "Test framework"
    "Some test for test framework"
    [  Unit.make_test "Atom test: and" "sucess" _test_and_S_S         
    ;  Unit.make_test "Atom test: and" "success then fail" _test_and_S_F         
    ;  Unit.make_test "Atom test: and" "fail then success" _test_and_F_S         
    ;  Unit.make_test "Atom test: and" "fail then fail" _test_and_F_F         
    ;  Unit.make_test "Atom test: and_list" "sucess" _test_and_S_l         
    ;  Unit.make_test "Atom test: and_list" "success then fail" _test_and_S_F_l       
    ;  Unit.make_test "Atom test: run hides failwith" "check that status fail stop further executions with run (subsequent terms are not executed)" _test_run_F_failwith  
    ;  Unit.make_test "Atom test: run" "fail then fail" _test_run_F_F         
    ;  Unit.make_test "Atom test: run" "success then fail" _test_run_S_F         
    ;  Unit.make_test "Atom test: run" "success" _test_run_S_S         
    ]