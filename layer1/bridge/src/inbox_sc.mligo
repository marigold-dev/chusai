#import "../../commons/inbox_interface.mligo" "Inbox"
#import "../../commons/ticket_api_workaround.mligo" "Ticket"

type message = Inbox.message
(**The whole inbox, so the historic of all the inboxes since level 0 of the rollup**)
type messages = (nat, message list) big_map

type entrypoint = Inbox.entrypoint

type state = {
  rollup_level: nat
; ticket: Ticket.chusai_ticket option
; messages: (nat, message list) big_map
; fixed_payload: bytes
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


let deposit (state: state) (owner: address) (ticket: Ticket.chusai_ticket) : return =
  let (_, (payload, quantity)), fresh_ticket = Ticket.read_ticket ticket in
  if state.fixed_payload = payload then
    let message = make_deposit_message owner quantity in
    let joined_ticket = 
      match state.ticket with
      | None -> Some fresh_ticket
      | Some ticket -> Ticket.join_tickets ticket fresh_ticket in
    let new_messages = push_message state message in
    let new_state = { state with ticket = joined_ticket ; messages = new_messages} in
    ([], new_state)
  else
    failwith "Inbox_sx: The rollup does not support this kind of payload"


let main (action, state : entrypoint * state) : return =
  match action with
  | Inbox_deposit ticket ->
    let ticket_owner = Tezos.sender in
    deposit state ticket_owner ticket