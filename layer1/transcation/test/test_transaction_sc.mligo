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
#import "../src/transaction_sc.mligo" "Tx"
#import "../src/transaction.mligo" "Tx_lib"
#include "../../commons/transaction.mligo"

type originated = Unit.originated
type originated_tx = (Tx.parameter, Tx.storage) originated

let originate_tx_sc () : originated_tx =
    let empty_storage : Tx.storage =
        { is_expected_state = Nothing } in
    Unit.originate Tx.main empty_storage 0tez

let none () : chusai_contract_state  = None

let _test_exec_transaction_op () =
  (* setup *)
  let operator, actors = Unit.init_default () in
  let alice, bob, carol = actors in
  let tx_sc : originated_tx = Unit.act_as operator originate_tx_sc  in

  (* make parameter and run *)
  let tx_op = Tx_lib.make_only_quantity alice.address bob.address 1n  in
  let tx_param =
        { tx = tx_op
        ; init_states = (1n, none ()), (1n, none ())
        ; expected_states = (0n, none ()), (2n, none ())
        }
  in
  let perform_tx_op () = Unit.transfer_to_contract_ tx_sc.originated_contract (Transaction tx_param) 0tez in

  (* check result *)
  let result = Unit.act_as carol perform_tx_op in
  let storage = Test.get_storage tx_sc.originated_typed_address in
  Unit.and_list
  [ result
  ; Unit.assert_equals Yes (storage.is_expected_state) "the expected should be Yes"
  ]

let _test_transaction_op_with_wrong_expected_state () =
  (* setup *)
  let operator, actors = Unit.init_default () in
  let alice, bob, carol = actors in
  let tx_sc : originated_tx = Unit.act_as operator originate_tx_sc  in

  (* make parameter and run *)
  let tx_op = Tx_lib.make_only_quantity alice.address bob.address 1n  in
  let tx_param =
        { tx = tx_op
        ; init_states = (1n, none ()), (1n, none ())
        ; expected_states = (0n, none ()), (3n, none ())
        }
  in
  let perform_tx_op () = Unit.transfer_to_contract_ tx_sc.originated_contract (Transaction tx_param) 0tez in

  (* check result *)
  let result = Unit.act_as carol perform_tx_op in
  let storage = Test.get_storage tx_sc.originated_typed_address in
  Unit.and_list
  [ result
  ; Unit.assert_equals (No { error = "Unexpected transited state" }) (storage.is_expected_state) "the expected should be Yes"
  ]

let _test_exec_transaction_from_op () =
  (* setup *)
  let operator, actors = Unit.init_default () in
  let alice, bob, carol = actors in
  let tx_sc : originated_tx = Unit.act_as operator originate_tx_sc  in

  (* make parameter and run *)
  let tx_op = Tx_lib.make_only_quantity alice.address bob.address 1n  in
  let tx_from, _ = Tx_lib.to_small_step tx_op in
  let tx_param =
        { tx = tx_from
        ; init_state = (1n, none ())
        ; expected_state = (0n, none ())
        }
  in
  let perform_tx_op () = Unit.transfer_to_contract_ tx_sc.originated_contract (Transaction_from tx_param) 0tez in

  (* check result *)
  let result = Unit.act_as carol perform_tx_op in
  let storage = Test.get_storage tx_sc.originated_typed_address in
  Unit.and_list
  [ result
  ; Unit.assert_equals Yes (storage.is_expected_state) "the expected should be Yes"
  ]

let _test_exec_transaction_from_op_with_unexpected_state () =
  (* setup *)
  let operator, actors = Unit.init_default () in
  let alice, bob, carol = actors in
  let tx_sc : originated_tx = Unit.act_as operator originate_tx_sc  in

  (* make parameter and run *)
  let tx_op = Tx_lib.make_only_quantity alice.address bob.address 1n  in
  let tx_from, _ = Tx_lib.to_small_step tx_op in
  let tx_param =
        { tx = tx_from
        ; init_state = (1n, none ())
        ; expected_state = (6n, none ())
        }
  in
  let perform_tx_op () = Unit.transfer_to_contract_ tx_sc.originated_contract (Transaction_from tx_param) 0tez in

  (* check result *)
  let result = Unit.act_as carol perform_tx_op in
  let storage = Test.get_storage tx_sc.originated_typed_address in
  Unit.and_list
  [ result
  ; Unit.assert_equals (No { error = "Unexpected transited state" }) (storage.is_expected_state) "the expected should be Yes"
  ]

let _test_exec_transaction_to_op () =
  (* setup *)
  let operator, actors = Unit.init_default () in
  let alice, bob, carol = actors in
  let tx_sc : originated_tx = Unit.act_as operator originate_tx_sc  in

  (* make parameter and run *)
  let tx_op = Tx_lib.make_only_quantity alice.address bob.address 1n  in
  let _, tx_to = Tx_lib.to_small_step tx_op in
  let tx_param =
        { tx = tx_to
        ; init_state = (1n, none ())
        ; expected_state = (2n, none ())
        }
  in
  let perform_tx_op () = Unit.transfer_to_contract_ tx_sc.originated_contract (Transaction_to tx_param) 0tez in

  (* check result *)
  let result = Unit.act_as carol perform_tx_op in
  let storage = Test.get_storage tx_sc.originated_typed_address in

  Unit.and_list
  [ result
  ; Unit.assert_equals Yes (storage.is_expected_state) "the expected should be Yes"
  ]

let _test_exec_transaction_to_op_with_unexpected_state () =
  (* setup *)
  let operator, actors = Unit.init_default () in
  let alice, bob, carol = actors in
  let tx_sc : originated_tx = Unit.act_as operator originate_tx_sc  in

  (* make parameter and run *)
  let tx_op = Tx_lib.make_only_quantity alice.address bob.address 1n  in
  let _, tx_to = Tx_lib.to_small_step tx_op in
  let tx_param =
        { tx = tx_to
        ; init_state = (1n, none ())
        ; expected_state = (6n, none ())
        }
  in
  let perform_tx_op () = Unit.transfer_to_contract_ tx_sc.originated_contract (Transaction_to tx_param) 0tez in

  (* check result *)
  let result = Unit.act_as carol perform_tx_op in
  let storage = Test.get_storage tx_sc.originated_typed_address in

  Unit.and_list
  [ result
  ; Unit.assert_equals (No { error = "Unexpected transited state" }) (storage.is_expected_state) "the expected should be Yes"
  ]

let suite = Unit.make_suite
  "Transaction smart contract"
  "Test suite of transaction smart contract"
  [ Unit.make_test "perform token transfer operation" "test transaction operation" _test_exec_transaction_op
  ; Unit.make_test
      "perform transfer operation with wrong expected states"
      "test transaction operation failed"
      _test_transaction_op_with_wrong_expected_state
  ; Unit.make_test
     "source: perform token transfer operation"
     "test small step: transaction_from operation"
     _test_exec_transaction_from_op
  ; Unit.make_test
      "source: perform token transfer operation with wrong expected states"
      "test small step: test transaction operation failed"
     _test_exec_transaction_from_op_with_unexpected_state
  ; Unit.make_test
     "destination: perform token transfer operation"
     "test small step: transaction_from operation"
     _test_exec_transaction_to_op
  ; Unit.make_test
      "destination: perform token transfer operation with wrong expected states"
      "test small step: test transaction operation failed"
     _test_exec_transaction_to_op_with_unexpected_state
  ]
