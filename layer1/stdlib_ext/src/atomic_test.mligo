#import "stdlibext.mligo" "Stdlib"
(* types *)

type gas = nat

type failure_reason 
  = Message of string
  | Execution of test_exec_error

type status
  = Status_Fail of failure_reason
  | Status_Success of gas
    
(* Constructors *)

let succeed () = Status_Success 0n
let fail (msg:string) = Status_Fail (Message msg)
let start = Status_Success 0n

let is_failure (s:status):bool = 
    match s with 
    | Status_Success _ -> false
    | _ -> true

(* Combining/Chaining/Propagation of status *)

let and (s1:status) (s2:status) = 
    match s1,s2 with
    | Status_Success g1, Status_Success g2 -> Status_Success (g1 + g2)
    | Status_Success _, _ -> s2
    | _,_ -> s1
        
let and_list (l:status list) = 
    let and_ (s1, s2 : status * status) = and s1 s2 in
    List.fold_left and_ (succeed ()) l

let run (current:status)  (op: unit -> status)   =
    match current with
    | Status_Success _ -> and current (op ())
    | _ -> current

(* Wrapping of Test module *)

let wrap_exec (result:test_exec_result) = 
    match result with
    | Success g -> Status_Success g
    | Fail error -> Status_Fail ( Execution error)

let transfer_to_contract_ (type param) (contr : param contract) (action:param) (amount_:tez) =
    wrap_exec (Test.transfer_to_contract contr action amount_)

let transfer_to_contract (type param) (current:status) (contr : param contract) (action:param) (amount_:tez) =
    run current (fun () -> transfer_to_contract_ contr action amount_)

(* ASSERTIONS *)

let assert_  (b:bool) (msg:string)  : status = 
    if b then succeed () else fail msg

let assert_is_ok (current:status) (msg:string) : status = 
    match current with
    | Status_Fail _ -> fail msg
    | Status_Success _ -> current

let assert_exec_error (current:status) (predicate:test_exec_error -> bool) (msg:string) = 
    match current with
    | Status_Fail (Execution e) -> assert_ (predicate e) msg
    | _ -> fail msg

let assert_rejected (current:status) (msg:string) : status =
    let predicate (e:test_exec_error) = match e with Rejected _ -> true | _ -> false in
    assert_exec_error current predicate msg

let assert_rejected_at (current:status) (addr:address) (msg:string) : status = 
    let predicate (e:test_exec_error) = match e with Rejected (_, contr_rejecting) -> addr = contr_rejecting |_ -> false in
    assert_exec_error current predicate msg

let assert_rejected_with_error (current:status) (expected_error:michelson_program) (msg:string) = 
  let predicate (e:test_exec_error) = 
    match e with  
    | Rejected (error , _) -> error = expected_error
    | _ -> false
  in
  assert_exec_error current predicate msg

let equals_ (type a) (actual : a) (expected : a) : bool = 
    Test.michelson_equal (Test.compile_value actual) (Test.compile_value expected)

let assert_equals (type a) (actual : a) (expected : a)  (msg:string) : status =
  assert_ (equals_ actual expected) msg


let assert_cond (type a) (actual : a) (predicate : a -> bool)  (msg:string) : status =
    assert_ (predicate actual) msg

(* ********************************** *)
(* RUNNER *)

(* types *)
(* Constructors are provided, should not be created/deconstructed directly *)

(* a test to be run *)
type test = { 
  name: string 
; desc: string
; action : (unit -> status)
}

(* a list of test to be run *)
type test_suite = {
  suite_name: string
; tests: test list
}

(* the result of a test *)
type test_result = {
  test_name: string
; test_desc: string
; test_result: status
}

(* Constructors *)

let make_test (name:string) (desc:string) (action:unit -> status) = {
  name = name
; desc = desc
; action = action
}

let make_suite (name:string) (tests: test list) = {
  suite_name = name
; tests = tests
}

(* log & pretty printing *)

let log_OK (type a) (msg:string) (info:a) = Test.log ("ok "^msg, info)

let log_KO (type a) (msg:string) (info:a) = Test.log ("KO "^msg, info)

let pp_test_result ({ test_name; test_desc; test_result }:test_result) = 
  match test_result with 
  | Status_Success g -> log_OK ("   " ^test_name ^ " - "^ test_desc) g
  | Status_Fail reason -> log_KO ("-> " ^test_name^" - " ^ test_desc) reason

(* running a test *)    

let perform_test ({ name; desc; action }:test) = 
  let result = action () in 
  { test_name = name; test_desc = desc; test_result = result }

let run_test_suite_gen (pp: test_result -> unit) (suite: test_suite) = 
  List.fold_left 
    (fun (flag, test : bool * test ) -> 
      begin
        let result = perform_test test in
        pp result;
        flag || (is_failure result.test_result)
      end) 
    false 
    suite.tests 

let run_test_suite (suite: test_suite) = 
  run_test_suite_gen pp_test_result suite

let run_suites_gen (pp: test_result -> unit) (suites: test_suite list) = 
  let final_result = List.fold_left (fun (flag, suite : bool * test_suite) -> 
      begin
        let result = run_test_suite_gen pp suite in 
        let _ = 
          if result then (log_KO suite.suite_name "Has failed test(s)")
          else (log_OK suite.suite_name "All tests passed")
        in
        flag || result
       end
    ) false suites
    in if final_result then failwith "Errors" else ()

    
let run_suites (suites: test_suite list) = 
  run_suites_gen pp_test_result suites

(* ********************************** *)
(* METRICS *)
  
let pp_test_result_metric ({ test_name; test_desc; test_result }:test_result) = 
  let _ = Test.log "START METRIC" in
  let _ = match test_result with 
    | Status_Fail error -> 
      let _ = Test.log (Stdlib.StringExt.concat_all " : " [test_name; "ERROR"]) in
      Test.log error
    | Status_Success gas_used ->  
      let _ = Test.log (Stdlib.StringExt.concat_all " : " [test_name; "SUCCESS"]) in
      Test.log gas_used in
  Test.log "END METRIC"

let run_test_suite_metric (suite: test_suite) = 
  run_test_suite_gen pp_test_result_metric suite

let run_suites_metrics  (suites: test_suite list) = 
  run_suites_gen pp_test_result_metric suites