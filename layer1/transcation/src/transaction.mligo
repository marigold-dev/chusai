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
    | Non_existed_source
    | Non_existed_destination
    | Non_existed_arg

  (** [result] is short for Stdlib_Result.t *)
  type result = Stdlib_Result.t

  (** [t] is the type of result value, which is
      either [chusai_states] if ok or [error] if error *)
  type t = (chusai_states, error) result

  (** get [states] or raise error *)
  let get_states_exn : t -> (error -> string) -> chusai_states = Stdlib_Result.get_ok_or_raises

  (** print error message *)
  let error_to_string (e : error)
    = match e with
    | Not_enough_balance -> "Not_enough_balance"
    | Non_existed_source -> "Non_existed_source"
    | Non_existed_destination -> "Non_existed_destination"
    | Non_existed_arg -> "Non_existed_arg"
end

(** [update_source_state] takes a transaction operation and [states]
    to update the state of [transaction.source] in [states] *)
let update_source_state ({ source; destination = _; quantity; arg = _} : Tx.transaction) (states : Tx.chusai_states) : Result.t =
  match Map.find_opt source states with
  | None -> Error Non_existed_source
  | Some (balance, contract_related) ->
    begin
      let new_balance = balance - quantity in
      if new_balance < 0 then
        Error Not_enough_balance
      else
        Ok (Map.update source (Some (abs(new_balance), contract_related)) states)
    end

(** [update_destination_balance] takes a transaction operation and [states]
    to update the state of [transaction.destination] in [states] *)
let update_destination_balance ({ source = _; destination; quantity; arg = _} : Tx.transaction) (states : Tx.chusai_states) : Result.t =
  match Map.find_opt destination states with
  | None -> Error Non_existed_destination
  | Some (balance, contract_related) ->
    begin
      let new_balance = balance + quantity in
      Ok (Map.update destination (Some (new_balance, contract_related)) states)
    end

(** [update_destination_contract] takes a transaction operation and [states]
    to update the state of [transaction.destination] in [states] *)
(** FIXME: implement in other PR *)
(** FIXME: check destination type (?) *)
let update_destination_contract ({ source = _; destination; quantity = _; arg} : Tx.transaction) (states : Tx.chusai_states) : Result.t =
  match arg, Map.find_opt destination states with
  | None, _ -> Error Non_existed_arg
  | _, None -> Error Non_existed_destination
  | Some _arg, Some (_balance, _contract_related) -> Ok states (* TODO *)
