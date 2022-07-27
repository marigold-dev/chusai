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

#import "../../stdlib_ext/src/result.mligo" "Stdlib_Result"
#import "../../commons/transaction.mligo" "Tx"

(** transaction lib *)

(** [Result] module provides info on failure *)
module Result = struct
  (** transaction errors *)
  type error
    = Not_enough_balance
    | Non_existed_arg
    | Not_implement_yet

  (** [result] is short for Stdlib_Result.t *)
  type result = Stdlib_Result.t

  (** [t] is the type of result value, which is
      either [chusai_states] if ok or [error] if error *)
  type t = (Tx.chusai_state, error) result

  (** get [states] or raise error *)
  let get_states_exn : t -> (error -> string) -> Tx.chusai_state = Stdlib_Result.get_ok_or_raises

  (** print error message *)
  let error_to_string (e : error)
    = match e with
    | Not_enough_balance -> "Not_enough_balance"
    | Non_existed_arg -> "Non_existed_arg"
    | Not_implement_yet -> "Not_implement_yet"
end

(** create a [transaction] operation *)
let make (source : address) (destination : address)
      (quantity : Tx.chusai_balance) (arg : bytes option) : Tx.transaction =
  { source = source; destination = destination; quantity = quantity; arg = arg }

(** create a [transaction] operation without [arg] for
    transfering [quantity] only *)
let make_only_quantity (source : address) (destination : address)
      (quantity : Tx.chusai_balance) : Tx.transaction =
  { source = source; destination = destination; quantity = quantity; arg = None }

(** split [transaction] to [transaction_from] and [transaction_to] in small steps *)
let to_small_step ({source; destination; quantity; arg; } : Tx.transaction) : (Tx.transaction_from * Tx.transaction_to) =
  ( {source = source; quantity = quantity; }
  , {destination = destination; quantity = quantity; arg = arg})

(** [update_source_state] takes a transaction operation and [chusai_state]
    to update the [chusai_state]. *)
let update_source_state ({ source = _; quantity; } : Tx.transaction_from) (balance, contract_related : Tx.chusai_state) : Result.t =
  let new_balance = balance - quantity in
  if new_balance < 0 then
    Error Not_enough_balance
  else
    Ok (abs(new_balance), contract_related)

(** [update_destination_balance] takes a transaction operation and [chusai_states]
    to update the [chusai_state] *)
let update_destination_balance ({ destination = _; quantity; arg = _} : Tx.transaction_to) (balance, contract_related : Tx.chusai_state) : Result.t =
  let new_balance = balance + quantity in
  Ok (new_balance, contract_related)

(** [update_destination_contract] takes a transaction operation and [chusai_state]
    to update the state of [transaction.destination] in [chusai_state] *)
(** FIXME: implement in other PR *)
(** FIXME: check destination type (?) *)
let update_destination_contract (_t : Tx.transaction) (state : Tx.chusai_state) : Result.t =
  Error Not_implement_yet (* TODO *)
