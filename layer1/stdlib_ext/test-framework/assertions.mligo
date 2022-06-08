
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

(* ASSERTIONS *)
let assert_  (b:bool) (msg:string)  : result = 
    if b then succeed () else fail msg

let assert_is_ok (current:result) (msg:string) : result = 
    match current with
    | Test_Failed _ -> fail msg
    | Test_Passed _ -> current

let assert_exec_error (current:result) (predicate:test_exec_error -> bool) (msg:string) = 
    match current with
    | Test_Failed (Execution e) -> assert_ (predicate e) msg
    | _ -> fail msg

let assert_rejected (current:result) (msg:string) : result =
    let predicate (e:test_exec_error) = match e with Rejected _ -> true | _ -> false in
    assert_exec_error current predicate msg

let assert_rejected_at (current:result) (addr:address) (msg:string) : result = 
    let predicate (e:test_exec_error) = match e with Rejected (_, contr_rejecting) -> addr = contr_rejecting |_ -> false in
    assert_exec_error current predicate msg

let assert_rejected_with_error (current:result) (expected_error:michelson_program) (msg:string) = 
  let predicate (e:test_exec_error) = 
    match e with  
    | Rejected (error , _) -> error = expected_error
    | _ -> false
  in
  assert_exec_error current predicate msg

(** A deep equality that relay on the Michelson representation. *)
let michelson_equal (type a) (given: a) (expected: a) : bool =
  let michelson_given = Test.compile_value given in
  let michelson_expected = Test.compile_value expected in
  Test.michelson_equal michelson_given michelson_expected

let assert_equals (type a) (actual : a) (expected : a)  (msg:string) : result =
  assert_ (michelson_equal actual expected) msg


let assert_cond (type a) (actual : a) (predicate : a -> bool)  (msg:string) : result =
    assert_ (predicate actual) msg
