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

type is_expected_states
  = Yes
  | No of { error : Result.error; }
  | Nothing

(** storage type *)
(** [FIXME]: support multiple game *)
type storage =
  { referee : address
  ; is_expected_states : is_expected_states
  ;
  }

(** parameter of Transaction *)
type tx_parameter =
  { tx : transaction
  ; init_states : chusai_states
  ; expected_states : chusai_states
  ;
  }

(** parameter *)
type parameter =
  Transaction of tx_parameter

(** return *)
type return = operation list * storage

(** compare a actual state and a expected states *)
let check_expected_state (result : Result.t) (expected_states : chusai_states) : is_expected_states = (
  match result with
  | Error e -> No { error = e; }
  | Ok states -> (
      let b_expected_state = Bytes.pack expected_states in
      let b_actual_state = Bytes.pack states in
      if b_expected_state = b_actual_state then Yes
      else Nothing))

(** perform transaction *)
let transaction ({ tx; init_states; expected_states; } : tx_parameter ) : Result.t =
  let result = Tx.update_source_state tx init_states in
  match result with
  | Error _ -> result
  | Ok states -> Tx.update_destination_balance tx states

(** Main *)
let main(action, storage : parameter * storage) : return =
  let checked =
    match action with
    | Transaction t ->
      begin
        let result =
          if storage.referee <> Tezos.get_sender ()
          then (Error Wrong_referee : Result.t)
          else transaction t
        in
        check_expected_state result t.expected_states
      end
  in
  ([], {referee = storage.referee; is_expected_states = checked })

