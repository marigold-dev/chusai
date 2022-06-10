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
   
(* ORDER OF INCLUDES MATTERS *)
#include "../test-framework/context.mligo"
#include "../test-framework/result.mligo" 
// these ones are imports just to emphasize the api 
#import "../test-framework/engine.mligo" "Engine" 
#import "../test-framework/metrics.mligo" "Metrics" 
#include "../test-framework/assertions.mligo"
#include "../test-framework/wrap_test.mligo"
#include "../test-framework/contract.mligo"
    
// redefinitions for the sake of clarity : these are the only function in module Engine that matters
let make_test = Engine.make_test 
let make_suite = Engine.make_suite
let run_suites = Engine.run_suites
let run_suites_metrics = Metrics.run_suites_metrics
type test_suite = Engine.test_suite