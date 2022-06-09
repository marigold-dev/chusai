#import "../src/atomic_test.mligo" "Atom"

let _test_and_S_S () = 
    let b = Atom.and (Status_Success 1n) (Status_Success 2n) = Status_Success 3n in
    Atom.assert_ b "Should have been success"

let _test_and_S_F () = 
    let b = Atom.and (Status_Success 1n) (Status_Fail  (Message "shown")) = Status_Fail (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"

let _test_and_F_S () = 
    let b = Atom.and (Status_Fail (Message "shown")) (Status_Success 1n) = Status_Fail (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"

let _test_and_F_F () = 
    let b = Atom.and (Status_Fail (Message "shown")) (Status_Fail (Message "hidden")) = Status_Fail (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"

let _test_and_S_l () = 
    let b = Atom.and_list 
    [ Status_Success 1n
    ; Status_Success 2n
    ; Status_Success 3n
    ; Status_Success 5n
    ] = Status_Success 11n in
    Atom.assert_ b "Should have been fail 'shown'"

let _test_and_S_F_l () = 
    let b = Atom.and_list 
    [ Status_Success 1n
    ; Status_Fail (Message "shown")
    ; Status_Fail (Message "hidden")
    ; Status_Success 5n
    ] = Status_Fail (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"

(* run allows partial evaluation : if the last known status is "fail" then subsequent expressions are not evaluated*)
let _test_run_F_failwith () = 
    let to_run = (fun () -> (begin failwith ""; Status_Fail (Message "hidden") end)) in
    let b = Atom.run (Status_Fail (Message "shown")) to_run = Status_Fail (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"

let _test_run_F_F () = 
    let to_run = (fun () -> Status_Fail (Message "hidden") ) in
    let b = Atom.run (Status_Fail (Message "shown")) to_run = Status_Fail (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"
    
let _test_run_S_F () = 
    let to_run = (fun () -> Status_Fail (Message "shown") ) in 
    let b = Atom.run (Status_Success 1n) to_run = Status_Fail (Message "shown") in
    Atom.assert_ b "Should have been fail 'shown'"

    
let _test_run_S_S () = 
    let to_run = (fun () -> Status_Success 2n ) in
    let b = Atom.run (Status_Success 1n) to_run = Status_Success 3n in
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