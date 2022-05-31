#import "../src/atomic_test.mligo" "Atom"

let _test_and_S_S () = 
    Atom.and (Status_Success 1n) (Status_Success 2n) = Status_Success 3n

let _test_and_S_F () = 
    Atom.and (Status_Success 1n) (Status_Fail "shown") = Status_Fail "shown"

let _test_and_F_S () = 
    Atom.and (Status_Fail "shown") (Status_Success 1n) = Status_Fail "shown"

let _test_and_F_F () = 
    Atom.and (Status_Fail "shown") (Status_Fail "hidden") = Status_Fail "shown"

let _test_and_S_l () = 
    Atom.and_list 
    [ Status_Success 1n
    ; Status_Success 2n
    ; Status_Success 3n
    ; Status_Success 5n
    ] = Status_Success 11n

let _test_and_S_F_l () = 
    Atom.and_list 
    [ Status_Success 1n
    ; Status_Fail "shown"
    ; Status_Fail "hidden"
    ; Status_Success 5n
    ] = Status_Fail "shown"

(* run allows partial evaluation : if the last known status is "fail" then subsequent expressions are not evaluated*)
let _test_run_F_failwith () = 
    let to_run = (fun () -> (begin failwith ""; Status_Fail "hidden" end)) in
    Atom.run (Status_Fail "shown") to_run = Status_Fail "shown"

let _test_run_F_F () = 
    let to_run = (fun () -> Status_Fail "hidden" ) in
    Atom.run (Status_Fail "shown") = Status_Fail "shown"
    
let _test_run_S_F () = 
    let to_run = (fun () -> Status_Fail "shown" ) in 
    Atom.run (Status_Success 1n) to_run = Status_Fail "shown"

    
let _test_run_S_S () = 
    let to_run = (fun () -> Status_Success 2n ) in
    Atom.run (Status_Success 1n) to_run = Status_Success 3n

let _test_assert_T () = 
    Atom.assert (Status_Success 42n) true "should have propagated success" = Status_Success 42n

let tests = Atom.run_tests_bool
    [   _test_and_S_S         
    ;   _test_and_S_F         
    ;   _test_and_F_S         
    ;   _test_and_F_F         
    ;   _test_and_S_l         
    ;   _test_and_S_F_l       
    ;   _test_run_F_failwith  
    ;   _test_run_F_F         
    ;   _test_run_S_F         
    ;   _test_run_S_S         
    ;   _test_assert_T        
    ]