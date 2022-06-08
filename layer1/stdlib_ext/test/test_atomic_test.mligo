#import "../src/atomic_test.mligo" "Atom"

let _test_and_S_S () = 
    let b = Atom.and (Test_Passed 1n) (Test_Passed 2n) = Test_Passed 3n in
    Atom.assert_ b "Should have been success"

let _test_and_S_F () = 
    let b = Atom.and (Test_Passed 1n) (Test_Failed  (Message "shown")) = Test_Failed (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"

let _test_and_F_S () = 
    let b = Atom.and (Test_Failed (Message "shown")) (Test_Passed 1n) = Test_Failed (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"

let _test_and_F_F () = 
    let b = Atom.and (Test_Failed (Message "shown")) (Test_Failed (Message "hidden")) = Test_Failed (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"

let _test_and_S_l () = 
    let b = Atom.and_list 
    [ Test_Passed 1n
    ; Test_Passed 2n
    ; Test_Passed 3n
    ; Test_Passed 5n
    ] = Test_Passed 11n in
    Atom.assert_ b "Should have been fail 'shown'"

let _test_and_S_F_l () = 
    let b = Atom.and_list 
    [ Test_Passed 1n
    ; Test_Failed (Message "shown") 
    ; Test_Failed (Message "hidden")
    ; Test_Passed 5n
    ] = Test_Failed (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"

(* run allows partial evaluation : if the last known status is "fail" then subsequent expressions are not evaluated*)
let _test_run_F_failwith () = 
    let to_run = (fun () -> (begin failwith ""; Test_Failed (Message "hidden") end)) in
    let b = Atom.and_lazy (Test_Failed (Message "shown")) to_run = Test_Failed (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"

let _test_run_F_F () = 
    let to_run = (fun () -> Test_Failed (Message "hidden") ) in
    let b = Atom.and_lazy (Test_Failed (Message "shown")) to_run = Test_Failed (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"
    
let _test_run_S_F () = 
    let to_run = (fun () -> Test_Failed (Message "shown") ) in 
    let b = Atom.and_lazy (Test_Passed 1n) to_run = Test_Failed (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"

    
let _test_run_S_S () = 
    let to_run = (fun () -> Test_Passed 2n ) in
    let b = Atom.and_lazy (Test_Passed 1n) to_run = Test_Passed 3n in
    Atom.assert_ b "Should have been Success 3n"


let suite = Atom.make_suite
    "Some test for atomic_test library"
    [  Atom.make_test "Atom test: and" "sucess" _test_and_S_S         
    ;  Atom.make_test "Atom test: and" "success then fail" _test_and_S_F         
    ;  Atom.make_test "Atom test: and" "fail then success" _test_and_F_S         
    ;  Atom.make_test "Atom test: and" "fail then fail" _test_and_F_F         
    ;  Atom.make_test "Atom test: and_list" "sucess" _test_and_S_l         
    ;  Atom.make_test "Atom test: and_list" "success then fail" _test_and_S_F_l       
    ;  Atom.make_test "Atom test: run hides failwith" "check that status fail stop further executions with run (subsequent terms are not executed)" _test_run_F_failwith  
    ;  Atom.make_test "Atom test: run" "fail then fail" _test_run_F_F         
    ;  Atom.make_test "Atom test: run" "success then fail" _test_run_S_F         
    ;  Atom.make_test "Atom test: run" "success" _test_run_S_S         
    ]