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

#import "../ticket/chusai_ticket.mligo" "Ticket"
#import "engine.mligo" "Internal"

type test_result = Internal.result

(** [is_true message bool] fail if the boolean is [false]. *)
let is_true (message: string) (assertion: bool) : test_result =
  if assertion then Internal.succeed
  else Internal.fail_with ("Assertion failed: " ^ message)

(** [is_true message bool] fail if the boolean is [true]. *)
let is_false (message: string) (assertion: bool) : test_result =
  is_true message (not assertion)

(** A deep equality that relay on the Michelson representation. *)
let michelson_equal (type a) (given: a) (expected: a) : bool =
  let michelson_given = Test.compile_value given in
  let michelson_expected = Test.compile_value expected in
  Test.michelson_equal michelson_given michelson_expected

(** [are_equal message a b] fail if the two given values are not equal. *)
let are_equal_michelson
    (type a)
    (message: string)
    (given: a)
    (expected: a) : test_result =
  let result = michelson_equal given expected in
  is_true message result

(** [conditional message x p] fail if [p x] is [false]. *)
let conditional
    (type a)
    (message: string)
    (input: a)
    (predicate: a -> bool) : test_result =
  let result = predicate input in
  is_true message result


(** [is_some message opt] fail if an option is unfilled. *)
let is_some (type a) (message: string) (opt: a option) : test_result =
  match opt with
  | None -> is_true message false
  | Some _ -> Internal.succeed

(** [is_none message opt] fail if an option is filled. *)
let is_none (type a) (message: string) (opt: a option) : test_result =
  match opt with
  | Some _ -> is_true message false
  | None -> Internal.succeed

(** the composition of [is_some] and [conditional]. *)
let is_some_when
    (type a)
    (message: string)
    (predicate: a -> bool)
    (opt: a option) : test_result =
  match opt with
  | None -> is_true message false
  | Some x -> conditional message x predicate



(** [replace_message msg subject] if [subject] is a failure, it will replace
    the inner message. *)
let replace_message (message: string) (subject: test_result) : test_result =
  match subject with
  | Passed _ -> subject
  | Failed _ -> Internal.fail_with message

let expect_execution_error
    (message: string)
    (predicate: test_exec_error -> bool)
    (subject: test_result) : test_result =
  match subject with
  | Failed (Execution err) -> is_true message (predicate err)
  | _ -> Internal.fail_with  message

(** Expect an execution error by [Rejected]. *)
let expect_execution_rejected
    (message: string)
    (subject: test_result) : test_result =
  let predicate (err: test_exec_error) =
    match err with
    | Rejected _ -> true
    | _ -> false
  in expect_execution_error message predicate subject

(** Expect an execution error by [Rejected] at a specific [address]. *)
let expect_execution_rejected_at
    (message: string)
    (address: address)
    (subject: test_result) : test_result =
  let predicate (err: test_exec_error) =
    match err with
    | Rejected (_, rejected_contract) -> address = rejected_contract
    | _ -> false
  in expect_execution_error message predicate subject

(** Expect an execution error by [Rejected] at a specific [execution_error]. *)
let expect_rejection_with_error
    (message: string)
    (expected_error: michelson_program)
    (subject: test_result) : test_result =
  let predicate (err: test_exec_error) : bool =
    match err with
    | Rejected (error, _) -> let () = Test.log (error) in error = expected_error
    | _ -> false
  in expect_execution_error message predicate subject

(** Compare, using an an option as expected part, only if the option is
    filled. *)
let if_some
    (type a)
    (message: string)
    (equal: a -> a -> bool)
    (expected: a option)
    (given: a) : unit =
  match expected with
  | None -> ()
  | Some value ->
    let result = equal value given in
    assert_with_error result message

(** Assert values of a ticket.
    FIXME: this function is a hook and may fail so it breaks the
           atomiticity of test-running. *)
let ticket
    (address: address option)
    (payload: bytes option)
    (quantity: nat option)
    (given_ticket: Ticket.t): Ticket.t =
  let (given_address, (given_payload, given_quantity)), fresh_given_ticket =
    Ticket.read_ticket given_ticket
  in
  let () =
    if_some
      "Assert.ticket: wrong address"
      (fun (a: address) (b: address) -> a = b)
      address given_address
  in
  let () =
    if_some
      "Assert.ticket: wrong payload"
      (fun (a: bytes) (b: bytes) -> a = b)
      payload given_payload
  in
  let () =
    if_some
      "Assert.ticket: wrong quantity"
      (fun (a: nat) (b: nat) -> a = b)
      quantity given_quantity
  in
  fresh_given_ticket
