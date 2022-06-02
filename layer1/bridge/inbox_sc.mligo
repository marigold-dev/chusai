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
#import "../common/inbox_messages.mligo" "Message"
#import "../common/inbox_sc_entrypoints.mligo" "Entrypoint"

type message = Message.t
type entrypoint = Entrypoint.t

(** The inbox state of the rollup. *)
type state = {
  cursor: nat                            (** The cursor of the message in a Big_map *)
; ticket: Ticket.t option                (** The main-ticket. *)
; messages: (nat, message list) big_map  (** Inboxes indexed by cursors *)
; fixed_payload: bytes                   (** Same as [fixed_payload] into [mint_sc] *)
}

(** Build a new deposit message. *)
let make_deposit_message (owner: address) (quantity: nat) : message =
  Deposit { owner = owner; quantity = quantity }

(** Push a message into an Inbox at the current level (cursor) of the Rollup.
    Messages are consed (in order to move the weight of the computations on
    the layer2 side). *)
let push_message (state: state) (message: message) : (nat, message list) big_map =
  let current_cursor = state.cursor in
  let current_messages = state.messages in
  match Big_map.find_opt current_cursor current_messages with
  | None -> Big_map.add current_cursor [ message ] current_messages
  | Some xs -> Big_map.add current_cursor ( message :: xs ) current_messages


(** Store a deposit into the inbox at the current level (cursor).
    A deposit consists of merging a ticket with the main ticket (if their payloads match)
    and building a deposit message to add to the inbox.

    FIXME: How to get the ticket back if their payload do not match? *)
let make_deposit
    (state: state)
    (owner: address)
    (ticket: Ticket.t) : (operation list * state) =
  let (_, (payload, quantity)), fresh_ticket = Ticket.read_ticket ticket in
  if (state.fixed_payload = payload) then
    let message = make_deposit_message owner quantity in
    (* We can safely join tickets since the payload was already checked.
       (At least... I guess...) *)
    let joined_ticket = Ticket.may_join_tickets state.ticket fresh_ticket in
    let new_messages = push_message state message in
    let new_state = { state with ticket = joined_ticket ; messages = new_messages } in
    ([], new_state)
  else failwith "inbox_sc: The rollup does not support this kind of payload"


(** Main is the entrypoint of [wallet_sc] contract *)
let main (action, state : entrypoint * state) : operation list * state =
  match action with
  | Inbox_deposit ticket_to_deposit ->
    let ticket_owner = Tezos.sender in
    make_deposit state ticket_owner ticket_to_deposit
