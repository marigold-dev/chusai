#import "../src/atomic_test.mligo" "Atom"

let test_and_S_S = 
    (Atom.and (Status_Success 1n) (Status_Success 2n)) = (Status_Success 3n)


let test_and_S_F = 
    (Atom.and (Status_Success 1n) (Status_Fail "shown")) = (Status_Fail "shown")
let test_and_F_S = 
    (Atom.and (Status_Fail "shown") (Status_Success 1n) ) = (Status_Fail "shown")
let test_and_F_F = 
    (Atom.and (Status_Fail "shown") (Status_Fail "hidden") ) = (Status_Fail "shown")

(* run allows partial evaluation : if the last known status is "fail" then subsequent expressions are not evaluated*)
let test_run = 
    (Atom.run (Status_Fail "shown") (fun () -> (begin failwith ""; Status_Fail "hidden" end)))
        = (Status_Fail "shown")
        
(* and does NOT allow partial evaluation *)        
// let test_and_F_F = 
//     (Atom.and (Status_Fail "shown") (begin failwith ""; Status_Fail "hidden" end) ) = (Status_Fail "shown")

let test_assert_T = 
    (Atom.assert (Status_Success 42n) true "") = (Status_Success 42n)

