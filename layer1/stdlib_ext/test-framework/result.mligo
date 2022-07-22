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
#include "helper.mligo"
(** An convenient alias for describing gas *)
type gas = nat

(** The reasons that can lead to a failure.  *)
type failure_reason 
  = Message of string
  | Execution of test_exec_error

let pp_exec_error (e : test_exec_error) : string =
  match e with
  | Balance_too_low _btle -> "balance too low"
  | Rejected (_pgm,_address) -> "Rejected"
  | Other s -> s

let pp_failure (f : failure_reason) : string =
  match f with
  | Message s -> s
  | Execution e -> pp_exec_error e

(** The result of a test which can be passed or failed. If the test [passed]
    it returns the gas consumption *)
type result
  = Test_Failed of failure_reason
  | Test_Passed of gas
  
(** a convenient alias to describe a function returting a result *)
type test_action = unit -> result

(* Constructors *)

(** returns a [Passed] test result *)
let succeed () : result = Test_Passed 0n

(** returns a [Failed] test result *)
let fail_with (msg:string) : result = Test_Failed (Message msg)

(** a [Passed] test result, used as a seed for a sequence of actions *)
let start () : result = Test_Passed 0n

(* accessor *)

(** Test if a result is a [Test_Failed] *)
let is_failure (s : result) : bool = 
  match s with 
  | Test_Passed _ -> false
  | _ -> true
    
(** Combine two results *)
let and (left : result) (right : result) = 
  match left,right with
  | Test_Passed g1, Test_Passed g2 -> Test_Passed (g1 + g2)
  | Test_Passed _, x -> x
  | x, _ -> x 

(** extends `and` to lists *)        
let and_list (results : result list) = 
  let reducer (left, right : result * result) = and left right in
  List.fold_left reducer (succeed ()) results

(** lazy version of `and` *)
let and_lazy (left : result) (right : test_action) =
  if is_failure left then left
  else and left (right ())

(** extends lazy version of `and` on lists *)
let and_lazy_list (actions : test_action list) = 
  let reducer (left, right : result * test_action) = and_lazy left right in
  List.fold_left reducer (succeed ()) actions
