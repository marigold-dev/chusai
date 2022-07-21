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

#import "transaction.mligo" "Tx"
#include "../../commons/transaction.mligo"

module Result = Tx.Result

(* [is_expected_state] is for storage type *)
type is_expected_state
  = Yes
  | No of { error : string; }
  | Nothing

(** storage type *)
(** [FIXME]: support multiple game *)
type storage =
  {  is_expected_state : is_expected_state; }

(** parameter of Transaction *)
type tx_parameter =
  { tx : transaction
  ; init_states : chusai_states
  ; expected_states : chusai_states
  ;
  }

(** parameter of small step of transaction *)
type 't tx_step_parameter =
  { tx : 't
  ; init_state : chusai_state
  ; expected_state : chusai_state
  ;
  }

(** parameter *)
type parameter
  = Transaction of tx_parameter
  | Transaction_from of (transaction_from) tx_step_parameter
  | Transaction_to of (transaction_to) tx_step_parameter

(** return *)
type return = operation list * storage

(** compare a actual state and a expected states
    FIXME: replace with merkletree
*)
let check_expected_state (result : Result.t) (expected_state : chusai_state) : is_expected_state =
  match result with
  | Error e -> No { error = Result.error_to_string e; }
  | Ok state -> (
      let b_expected_state = Bytes.pack expected_state in
      let b_actual_state = Bytes.pack state in
      if b_expected_state = b_actual_state then Yes
      else No { error = "Unexpected transited state" })

(** transit states *)
let big_step_transit (tx : transaction) (state_src, state_dest : chusai_states) : (Result.t * Result.t option) =
  (** split in small steps *)
  let (tx_from, tx_to) = Tx.to_small_step tx in

  (** update the state of source *)
  let result = Tx.update_source_state tx_from state_src in

  (** update the state of destination *)
  match result with
  | Error _ -> result, None
  | Ok _ -> result, Some (Tx.update_destination_balance tx_to state_dest)

(** perform transaction action *)
let transaction (tx_parameter : tx_parameter) : storage =
   let (r_src, or_dest) = big_step_transit tx_parameter.tx tx_parameter.init_states in
   let (e_src, e_dest) = tx_parameter.expected_states in
   let check_src = check_expected_state r_src e_src in
   let checked =
     begin
       match check_src, or_dest with
       | Yes, Some r_dest -> check_expected_state r_dest e_dest
       | No _, _ -> check_src
       | _, _ -> failwith "can't be happened"
     end
   in ({is_expected_state = checked})

(** perform transaction_from action *)
let transaction_from ({tx; init_state; expected_state}: (transaction_from) tx_step_parameter) : storage =
  let result = Tx.update_source_state tx init_state in
  let checked = check_expected_state result expected_state in
  ({is_expected_state = checked})

(** perform transaction_to action *)
let transaction_to ({tx; init_state; expected_state}: (transaction_to) tx_step_parameter) : storage =
  let result = Tx.update_destination_balance tx init_state in
  let checked = check_expected_state result expected_state in
  ({is_expected_state = checked})

(** Main *)
let main(action, storage : parameter * storage) : return =
  let new_storage =
    match action with
    | Transaction t -> transaction t
    | Transaction_from t -> transaction_from t
    | Transaction_to t -> transaction_to t
  in
  ([], new_storage)
