(* MIT License

   Copyright (C) 2022 Marigold <contact@marigold.dev>

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

#import "../inbox_sc.mligo" "Inbox_sc"
#import "../../bootstrap/wallet_sc.mligo" "Wallet_sc"
#import "../../common/wallet_sc_entrypoints.mligo" "Wallet_entrypoints"
#import "../../common/inbox_sc_entrypoints.mligo" "Inbox_entrypoints"
#import "../../common/inbox_messages.mligo" "Message"
#import "../../common/ticket/chusai_ticket.mligo" "Ticket"
#import "../../common/test_framework/main.mligo" "Unit_test"

type originated = Unit_test.Contract.originated
type inbox_state = Inbox_sc.state
type inbox_entrypoint = Inbox_entrypoints.t
type message = Message.t
type wallet_entrypoint = Wallet_entrypoints.t
type wallet_state = Wallet_sc.state

let empty_state : inbox_state = {
    cursor = 0n
  ; ticket = None
  ; messages = (Big_map.empty : (nat, message list) big_map)
  ; fixed_payload = 0x00
  }

(** Originate [Inbox_sc]. *)
let originate_inbox
    (log_level: Unit_test.Logger.level) : (inbox_entrypoint, inbox_state) originated =
  Unit_test.Contract.originate
    log_level
    "Originated [Inbox_sc]"
    Inbox_sc.main
    empty_state
    0tez

(** Request deposit from Wallet_sc to Inbox_sc. *)
let deposit_ticket
    (previous: Unit_test.test_result)
    (wallet: (wallet_entrypoint, wallet_state) originated)
    (inbox: (inbox_entrypoint, inbox_state) originated) : Unit_test.test_result =
  Unit_test.transfer_to_contract
    previous
    wallet.originated_contract
    (Wallet_deposit inbox.originated_address)
    0tez


(** Compute the total balances using messages. *)
let compute_total_balance (inbox: (inbox_entrypoint, inbox_state) originated) : nat =
  let state = Test.get_storage inbox.originated_typed_address in
  let map = state.messages in
  let current = Big_map.find_opt 0n map in
  match current with
  | None -> 0n
  | Some messages ->
    List.fold_left (fun (acc, message: nat * message) ->
      match message with
      | Deposit { owner; quantity } -> acc + quantity
    ) 0n messages
