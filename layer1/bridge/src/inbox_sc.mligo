#import "../../commons/inbox_interface.mligo" "Inbox"
#import "../../commons/ticket_api_workaround.mligo" "Ticket"

#include "../../stdlib_ext/src/stdlibext.mligo"

type message = Inbox.message
(**The whole inbox, so the historic of all the inboxes since level 0 of the rollup**)
type messages = (nat, message list) big_map

type entrypoint = Inbox.entrypoint
type ticket_key = {
  ticketer: address
; payload: Ticket.chusai_payload
}

type state = {
  rollup_level: nat
; ticket: Ticket.chusai_ticket option
; fixed_ticket_key: ticket_key
; messages: (nat, message list) big_map
}

type return = operation list * state

let make_deposit_message (owner: address) (quantity: nat) : message =
  Deposit { owner = owner; quantity = quantity }


(** Push a message into the inbox, indexed by the current rollup level **)
let push_message (state: state) (message: message) : messages =
  let current_level = state.rollup_level in
  let current_messages = state.messages in
  match Big_map.find_opt current_level current_messages with
  | None -> Big_map.add current_level [ message ] current_messages
  | Some xs -> Big_map.add current_level ( message :: xs ) current_messages

let is_same_ticket_key (k1 : ticket_key) (ticketer, payload : address * Ticket.chusai_payload) : bool =
  k1.ticketer = ticketer && k1.payload = payload

let deposit (state: state) (owner: address) (ticket: Ticket.chusai_ticket) : return =
  let (addr, (payload, quantity)), fresh_ticket = Ticket.read_ticket ticket in
  let joined_ticket = 
    match state.ticket with
    | None -> 
      if is_same_ticket_key state.fixed_ticket_key (addr,payload) 
        then
          Some fresh_ticket
      else None
    | Some ticket ->
      Ticket.join_tickets ticket fresh_ticket in
  let _ = if (OptionExt.is_none joined_ticket) then failwith "Ticket payload is invalid" in
  let message = make_deposit_message owner quantity in
  let new_messages = push_message state message in
  let new_state = { state with ticket = joined_ticket ; messages = new_messages} in
  ([], new_state)


let main (action, state : entrypoint * state) : return =
  match action with
  | Inbox_deposit ticket ->
    let ticket_owner = Tezos.sender in
    deposit state ticket_owner ticket