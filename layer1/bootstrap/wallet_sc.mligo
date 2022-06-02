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

#import "../common/ticket/chusai_ticket.mligo" "Ticket"
#import "../common/wallet_sc_entrypoints.mligo" "Entrypoints"
#import "../common/mint_sc_entrypoints.mligo" "Mint_entrypoints"
#import "../common/inbox_sc_entrypoints.mligo" "Inbox_entrypoints"

(** Alias for entrypoint.  *)
type entrypoint = Entrypoints.t
type mint_entrypoint = Mint_entrypoints.t
type inbox_entrypoint = Inbox_entrypoints.t

(** The state/storage of the Wallet. Mint adress and Bridge adress are given on
    demand. So a Wallet can be used for multiple rollup and multiple mint, modulo
    Ticket's payload. *)
type state = {
  stored_ticket: Ticket.t option
}

(** Trigger the minting process to a [Mint_sc].  *)
let request_mint
    (state : state)
    (mint_address: address)
    (quantity: tez) : (operation list * state) =
  if quantity > 0tez
  then
    let callback_entrypoint : Ticket.t contract =
      (* FIXME: Refer to Ligo team *
         We cannot use [Entrypoint.to_string] because... /shrug *)
      Tezos.self "%wallet_retreive_ticket" in
    let mint_contract : mint_entrypoint contract =
      Tezos.get_contract_with_error
        mint_address
        ("wallet_sc: Unable to find Mint contract")
    in
    let operation =
      Tezos.transaction
        (Mint_mint callback_entrypoint)
        quantity
        mint_contract
    in
    ([operation], state)
  else
    failwith "wallet_sc: Amount of tez should be non-null"

(** A Callback action called from [Mint_sc] to join a freshly minted ticket with
    the stored one. *)
let retreive_ticket
    (state: state)
    (minted_ticket: Ticket.t) : operation list * state =
  let joined_ticket =
    Ticket.may_join_tickets
      state.stored_ticket
      minted_ticket
  in
  match joined_ticket  with
  | Some _ ->
    let new_state = { state with stored_ticket = joined_ticket } in
    ([], new_state)
  | None -> failwith "wallet_sc: Ticket payload is invalid"

(** Make a deposit to a rollup.
    FIXME: Allow to split the ticket to send only a part of the stored ticket. *)
let deposit
    (state: state)
    (bridge_address: address) : (operation list * state) =
  match state.stored_ticket with
  | Some ticket ->
    let bridge_contract : inbox_entrypoint contract =
      Tezos.get_contract_with_error
        bridge_address
        "wallet_sc: Unable to find Inbox contract"
    in
    let operation =
      Tezos.transaction
        (Inbox_deposit ticket)
        0tez
        bridge_contract
    in
    let new_ticket : Ticket.t option = None in
    let new_state = { state with stored_ticket = new_ticket } in
    ([operation], new_state)
  | None -> failwith "wallet_sc: No minted ticket"

(** Main is the entrypoint of [wallet_sc] contract *)
let main (action, state : entrypoint * state) : operation list * state =
  match action with
  | Wallet_request_mint mint_address ->
    let quantity = Tezos.amount in
    request_mint state mint_address quantity
  | Wallet_retreive_ticket minted_ticket ->
    retreive_ticket state minted_ticket
  | Wallet_deposit bridge_address ->
    deposit state bridge_address
