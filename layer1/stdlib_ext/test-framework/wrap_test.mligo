
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
   
(* Wrapping of Test module *)

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
  let succeed (n : nat) = Test_Passed n in
  let fail_with (err : test_exec_error) =
      Test_Failed (Execution err)
  in try_catch block succeed fail_with

(** wrap a call to [transfer to contract] into a test result, taking into account potential previous test result *)
let transfer_to_contract
    (type param)
    (previous: result)
    (contract: param contract)
    (action: param)
    (fund: tez) : result =
  let block () = Test.transfer_to_contract contract action fund in
  let operation () = try_with block in
  and_lazy previous operation

  
(** wrap a call to [transfer to contract] into a test result *)
let transfer_to_contract_
    (type param)
    (contract: param contract)
    (action: param)
    (fund: tez) : result =
    transfer_to_contract (start ()) contract action fund