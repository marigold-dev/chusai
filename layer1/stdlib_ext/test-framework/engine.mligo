
#include "status.mligo"
#import "../src/stdlibext.mligo" "Stdlib"
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