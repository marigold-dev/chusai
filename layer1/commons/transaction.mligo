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

(** Chusai contract *)
type chusai_contract = bytes

(** Storege of Chusai contract *)
type chusai_storage = bytes option

(** Number of operations emission from Chusai contract *)
type chusai_op_emission = nat

(** Return of Chusai contract *)
type chusai_contract_return = chusai_storage * chusai_op_emission

(** Balance of Chusai *)
type chusai_balance = nat


(** Chusai states represent ledgers, which consist of
    balance, contract, return from contract.

    TODO: replace with merkletree instead *)
type chusai_states = (address, chusai_balance * (chusai_contract_return * chusai_contract) option) map

(** [transaction] is an operation which means
    [source] transfers [quantity] amount to [destination],
    and applys a contract of [destination] with [arg]
    if the contract is existed.

    FIXME: needs signature
*)
type transaction =
  { source      : address
  ; destination : address
  ; quantity    : nat
  ; arg         : bytes option
  ;
  }

(** create a [transaction] operation *)
let make (source : address) (destination : address)
      (quantity : chusai_balance) (arg : bytes option) : transfer =
  { source = source; destination = destination; quantity = quantity; arg = arg }

(** create a [transaction] operation without [arg] for
    transfer amount only *)
let make_only_balance (source : address) (destination : address)
      (quantity : chusai_balance) : transfer =
  { source = source; destination = destination; quantity = quantity; arg = None }
