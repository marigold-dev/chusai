(* MIT License

   Copyright (C) 2022 Marigold <contact@marigold.dev>

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

(** The entry point of the test-framework.
    it should be using this: [#include "path/to/common/test/main.mligo"]. *)

#import "logger.mligo" "Logger"
#import "engine.mligo" "Internal"
#import "assertion.mligo" "Assert"
#import "contract.mligo" "Contract"

type test = Internal.test
type suite = Internal.suite
type test_result = Internal.result

(** Describes how to make suites and tests. *)
module Make =
  struct
    let a_test = Internal.a_test
    let a_suite = Internal.a_suite
  end

let and_then = Internal.and_then
let and_then_lazy = Internal.and_then_lazy
let reduce = Internal.reduce
let neutral = Internal.succeed
let fail_with = Internal.fail_with

let transfer_to_contract
    (type param)
    (previous: test_result)
    (contract: param contract)
    (action: param)
    (fund: tez) : test_result =
  let block () = Test.transfer_to_contract contract action fund in
  let operation () = Internal.try_with block in
  and_then_lazy previous operation

(** Run a list of test_suites.  *)
let run (log_level: Logger.level) (suites: suite list) : unit =
  Internal.run_suites log_level suites
