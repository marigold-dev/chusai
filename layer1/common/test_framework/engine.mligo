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

(** As the execution of a unit test causes a whole test suite to be stopped.
    This module allows you to describe a test framework that plays the entire
    test suite.

    The framework allows you to define unit tests and a collection of tests
    (called [test_suite]). *)

#import "logger.mligo" "Logger"
#import "helper.mligo" "Helper"

(** An conveinent alias for describing gas *)
type gas = nat

(** The reasons that can lead to a failure.  *)
type failure_reason =
  | Message of string
  | Execution of test_exec_error

(** The result of a test which can be passed or failed. If the test [passed]
    it returns the gas consumption *)
type result =
  | Passed of gas
  | Failed of failure_reason

(** A test is the conjunction of a [name], a [description] and an [action]. *)
type test = {
  test_name : string
; test_desc : string
; test_action : (Logger.level -> result) (* The absence of existential packing
                                            disallow to parametrize over a log
                                            function *)
}

(** A test suite is a collection of [tests] attached to a [name]
    and [description]. *)
type suite = {
  suite_name : string
; suite_desc : string
; suite_tests : test list
}

(** [a_test name desc f] create a test. *)
let a_test (name: string) (desc: string) (action: Logger.level -> result) : test = {
  test_name = name
; test_desc = desc
; test_action = action
}
(** FIXME: refer to Ligo team

    If I relay on type inference (and not using explicit return type)
    Input of the producer-function will not raise any error. *)

(** [a_suite name desc tests] create a test-suite.*)
let a_suite (name: string) (desc: string) (tests: test list) : suite = {
  suite_name = name
; suite_desc = desc
; suite_tests = tests
}

let succeed : result = Passed 0n
let fail_with (message: string) : result = Failed (Message message)

(** Check if a test is failed or not. *)
let is_failure (result: result) : bool =
  match result with
  | Passed _ -> false
  | Failed _ -> true

(** An attempt to pretty print the result of a test execution. *)
let log_test_result (name: string) (desc: string) (result: result) : unit =
  match result with
  | Passed gas_value ->
    let str_gas = Helper.int_to_string (int gas_value) in
    Test.log ("    [v] " ^ name ^ " - " ^ desc ^ " - with gas: " ^ str_gas ^ "g.")
  | Failed reason ->
    let () = Test.log ("    [x] " ^ name ^ " - " ^ desc) in
    Test.log reason

(** An attempt to pretty print the result of a test-suite execution. *)
let log_suite_result (is_failed: bool) (name : string) : unit =
    if is_failed then
      Test.log ("[x] <" ^ name ^ "> has some failures." )
    else
      Test.log ("[v] <" ^ name ^ "> has been succeed." )

(** Run a particular test (with a given log-level) and print the result
    on stdout. (and returns [false] if there is no errors) *)
let run_test (log_level: Logger.level) (test: test) : bool =
  let result = test.test_action log_level in
  let () = log_test_result test.test_name test.test_desc result in
  is_failure result

(** Run a particular test suite (with a given log-level) and print the
    result of each test on stdout. The function maintain a state if a
    test fail. *)
let run_suite (log_level: Logger.level) (suite: suite) : bool =
  let () = Test.log "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" in
  let () = Test.log (" + Running: " ^ suite.suite_name ^ " - " ^ suite.suite_desc) in
  let is_failed =
     List.fold_left (fun (flag, test : bool * test) ->
        let is_failed = run_test log_level test in
        flag || is_failed
     ) false suite.suite_tests
  in
  let () = log_suite_result is_failed suite.suite_name in
  is_failed

(** Run a set of suites and pretty print their results on stdout. if
    there is a failure, the function raise an error. *)
let run_suites (log_level: Logger.level) (suites: suite list) : unit =
  let is_failed =
    List.fold_left (fun (flag, suite : bool * suite) ->
      let is_failed = run_suite log_level suite in
      flag || is_failed
    ) false suites
  in
  if is_failed then
    failwith "Some suites has been failed"
  else ()

(** Make the monoidal product between two test results.  *)
let and_then (left: result) (right: result) : result =
  match (left, right) with
  | Passed gas_left, Passed gas_right -> Passed (gas_left + gas_right)
  | Passed _, x -> x | x, _ -> x

(** A lazy version of [and_then]. *)
let and_then_lazy (left: result) (right : unit -> result) : result =
  if is_failure left then left
  else and_then left (right ())

(** Reduce a list of results in once using monoidal products. *)
let reduce (results: result list) : result =
  let reducer (left, right: result * result) : result = and_then left right in
  List.fold_left reducer (Passed 0n) results

(** Try to perform a failable computation ([block]) and fold the two possible
    results. *)
let try_catch
    (type a)
    (block: unit -> test_exec_result)
    (capture: nat -> a)
    (catch: test_exec_error -> a) : a =
  let result = block () in
  match result with
  | Success gas_value -> capture gas_value
  | Fail err -> catch err

(** Wrap an execution result into a test result. *)
let try_with (block: unit -> test_exec_result) : result =
  let succeed (n : nat) = Passed n in
  let fail_with (err : test_exec_error) =
      Failed (Execution err)
  in try_catch block succeed fail_with
