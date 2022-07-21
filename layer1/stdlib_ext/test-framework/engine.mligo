(* MIT License

   Copyright (c) 2022 Marigold <contact@marigold.dev>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
   the Software, and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

#include "result.mligo"
#import "helper.mligo" "Helper"
#import "../src/stdlibext.mligo" "Stdlib"
(* ********************************** *)
(* types *)

(** A test is the conjunction of a [name], a [description] and an [action]. *)
type test = { 
  name: string 
; desc: string
; action : (test_action)
}

(** A test suite is a collection of [tests] attached to a [name]
    and [description]. *)
type test_suite = {
  suite_name: string
; suite_desc: string
; tests: test list
}

(** the result of a test *)
type test_result = {
  test_name: string
; test_desc: string
; test_result: result
}

(* ********************************* *)
(* Constructors *)

(** build a test *)
let make_test (name : string) (desc : string) (action : test_action) = {
  name = name
; desc = desc
; action = action
}

// FIXME: should hase description as parameter
(** build a test suite *)
let make_suite (name : string) (desc : string) (tests : test list) = {
  suite_name = name
; suite_desc = desc
; tests = tests
}
(* ********************************* *)
(* pretty printing *)

(** pretty printing of [Failed] test result *)
let pp_test_failed (name : string) (desc : string) (reason : failure_reason) : unit = 
    let () = Test.log ("    [X] " ^ name ^ " - " ^ desc) in
    Test.log reason

(** pretty printing of [Passed] test result *)
let pp_test_passed (name : string) (desc : string) (gas_value : gas) : unit = 
    let str_gas = Helper.int_to_string (int gas_value) in
    Test.log ("    [v] " ^ name ^ " - " ^ desc ^ " - with gas: " ^ str_gas ^ "g.")

(** pretty printing of test result *)
let pp_test_result ({ test_name; test_desc; test_result } : test_result) : unit = 
  match test_result with 
  | Test_Passed g -> pp_test_passed test_name  test_desc g
  | Test_Failed reason -> pp_test_failed test_name test_desc reason

(** A pretty printer called before executing a test suite*)
let pp_suite_preamble (suite : test_suite) : unit = 
  let () = Test.log "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" in
  Test.log (" + Running: " ^ suite.suite_name ^ " - " ^ suite.suite_desc)

(** pretty printing of the result of a test suite *)
let pp_suite_result (has_failures : bool) (suite : test_suite) : unit =
    if has_failures then
      Test.log ("[X] <" ^ suite.suite_name ^ "> has some failures." )
    else
      Test.log ("[v] <" ^ suite.suite_name ^ "> has passed." )

(** pretty printing of a test result obtained in the context of a [metric] *)
let pp_metric ({ test_name; test_desc = _; test_result } : test_result) : unit = 
  let _ = Test.log "START METRIC" in
  let _ = match test_result with 
    | Test_Failed error -> 
      let _ = Test.log (Stdlib.StringExt.concat_all " : " [test_name; "ERROR"]) in
      Test.log error
    | Test_Passed gas_used ->  
      let _ = Test.log (Stdlib.StringExt.concat_all " : " [test_name; "SUCCESS"]) in
      Test.log gas_used in
  Test.log "END METRIC"

(* ********************************* *)
(* running a test *)    

(** Run a particular test and print the result
    on stdout. (and returns [false] if there is no errors) *)
let run_test ({ name; desc; action } : test) = 
  let result = action () in 
  { test_name = name; test_desc = desc; test_result = result }


(** Run a particular test suite and print the
    result of each test using provided pretty printer. The function maintain a state if a
    test fail. *)
let run_test_suite_with_pp (pp : test_result -> unit) (suite : test_suite) : bool = 
  let () = pp_suite_preamble suite in
  let has_failures = 
  List.fold_left 
    (fun (flag, test : bool * test ) -> 
      begin
        let result = run_test test in
        pp result;
        flag || (is_failure result.test_result)
      end) 
    false 
    suite.tests 
    in 
    let () = pp_suite_result has_failures suite in
    has_failures

(** Run a particular test suite and print the
    result of each test on stdout. The function maintain a state if a
    test fail. *)
let run_test_suite (suite : test_suite) : bool = 
  run_test_suite_with_pp pp_test_result suite

(** Run a set of suites and pretty print their results using provided function. if
    there is a failure, the function raise an error. *)
let run_suites_with_pp (pp: test_result -> unit) (suites: test_suite list) = 
  let final_result = List.fold_left (fun (flag, suite : bool * test_suite) -> 
      begin
        let has_failures = run_test_suite_with_pp pp suite in 
        flag || has_failures
       end
    ) false suites
    in if final_result then 
      failwith "Some suites have failed"
    else ()

    
(** Run a set of suites and pretty print their results on stdout. if
    there is a failure, the function raise an error. *)
let run_suites (suites : test_suite list) = 
  run_suites_with_pp pp_test_result suites
