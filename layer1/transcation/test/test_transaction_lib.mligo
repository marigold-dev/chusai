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

#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#import "../../stdlib_ext/src/result.mligo" "Stdlib_Result"
#import "../src/transaction.mligo" "Tx"
#include "../../commons/transaction.mligo"

let alice = ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)
let bob= ("tz2MjURoJLrxUKgHKgH2TnbWGABxrjb4Z85F" : address )

let _test_make_tx_with_only_quantity () =
  let tx = Tx.make_only_quantity alice bob 1n in
  Unit.and_list
    [ Unit.assert_equals (None : bytes option) tx.arg "arg should be none"
    ]

let _test_to_small_step () =
  let tx = Tx.make alice bob 1n (Some 0x00) in
  let tx_from, tx_to = Tx.to_small_step tx in
  Unit.and_list
    [ Unit.assert_equals ({source = alice; quantity = 1n}) tx_from "to_small_step: testing separating transaction_from from transaction"
    ; Unit.assert_equals ({destination = bob; quantity = 1n; arg = (Some 0x00)}) tx_to "to_small_step: testing separating transaction_to from transaction"
    ]

let _test_update_source_state () =
  (* alice's state: she has 4n *)
  let alice_state = 4n, (None : (chusai_contract_return * chusai_contract) option) in

  (* tx op: alice transfers 3n to bob *)
  let tx = Tx.make_only_quantity alice bob 3n in

  let tx_from, _ = Tx.to_small_step tx in
  let result = Tx.update_source_state tx_from alice_state in
  let new_alice_state = Stdlib_Result.get_ok result in
  Unit.and_lazy_list
  [ fun () -> Unit.assert_ (Stdlib_Result.is_ok result) "should get new state"
  ; fun () -> Unit.assert_equals (1n, (None : (chusai_contract_return * chusai_contract) option)) new_alice_state "the balance of alice should be 1n"
  ]

let _test_update_source_state_failing () =
  (* alice's state: she has 4n *)
  let alice_state = 1n, (None : (chusai_contract_return * chusai_contract) option) in

  (* tx op: alice transfers 3n to bob *)
  let tx = Tx.make_only_quantity alice bob 3n in

  let tx_from, _ = Tx.to_small_step tx in

  Unit.and_lazy_list
  [ fun () -> let result = Tx.update_source_state tx_from alice_state in Unit.assert_ (Stdlib_Result.is_error result) "should get error since alice doesn't have enought token"

  ; fun () -> let result = Tx.update_source_state tx_from alice_state in Unit.assert_ (match result with | Error e -> Not_enough_balance = e | Ok _ -> false) "the error message should be Not_enough_balance"
  ]

let _test_update_destination_balance  () =
  (* bob's state: he has 2n *)
  let bob_state = 2n, (None : (chusai_contract_return * chusai_contract) option) in

  (* tx op: alice transfers 3n to bob *)
  let tx = Tx.make_only_quantity alice bob 3n in

  let _, tx_to = Tx.to_small_step tx in
  let result = Tx.update_destination_balance tx_to bob_state in
  let new_bob_state = Stdlib_Result.get_ok result in

  Unit.and_lazy_list
  [ fun () -> Unit.assert_ (Stdlib_Result.is_ok result) "should get new state"
  ; fun () -> Unit.assert_equals (5n, (None : (chusai_contract_return * chusai_contract) option)) new_bob_state "the balance of Bob should be 5n"
  ]

let suite = Unit.make_suite
  "Transaction"
  "Test suite of transaction lib"
  [ Unit.make_test "make token transfer operation" "test arg should be none" _test_make_tx_with_only_quantity
  ; Unit.make_test "transaction to small step" "test transaction to small step" _test_to_small_step
  ; Unit.make_test "eval transaction and update the state of source" "test the state of source is updated" _test_update_source_state
  ; Unit.make_test "eval transaction but source doesn't have enough token" "test the state of source update failing" _test_update_source_state_failing
  ; Unit.make_test "eval transaction and update the state of destination" "test the state of destination is updated" _test_update_destination_balance
  ]
