#import "../../commons/inbox_interface.mligo" "Inbox"
#import "../../commons/transaction.mligo" "Tx"
#import "../../commons/ticket/chusai_ticket.mligo" "Ticket"

#include "../../stdlib_ext/src/stdlibext.mligo"

type message = Inbox.message

type inbox_level = nat

(** Each [inbox_level] has its own inbox, which is a list of messages.
    The [inbox_level] is used in chain_sc and starts from 0.
    The inbox level 0 belongs to the genesis block so its inbox
    should have no message. *)
type inboxes = (inbox_level, message list) big_map

type entrypoint = Inbox.entrypoint

type ticket_key = {
  mint_address: address
; payload: Ticket.payload
}

type state = [@layout:comb] {
  max_inbox_level: inbox_level
; ticket: Ticket.t option
; fixed_ticket_key: ticket_key
; inboxes: inboxes
}

let make_deposit_message (owner: address) (quantity: nat) : message =
  Deposit { owner = owner; quantity = quantity }

let make_transaction_message (source: address) (destination: address) (quantity: nat) : message =
  Transaction { source = source ; destination = destination; quantity = quantity; arg = (None : bytes option) }

(** Push a message into the inbox, indexed by the current inbox level **)
let push_message (max_inbox_level: nat) (inboxes : inboxes) (message: message) : inboxes =
  let current_level = max_inbox_level in
  let current_inboxes = inboxes in
  match Big_map.find_opt current_level current_inboxes with
  | None -> Big_map.add current_level [ message ] current_inboxes
  | Some xs -> Big_map.add current_level ( message :: xs ) current_inboxes

let is_same_ticket_key (k1 : ticket_key) (mint_address, payload : address * Ticket.payload) : bool =
  k1.mint_address = mint_address && k1.payload = payload

let deposit (max_inbox_level : nat) (state_ticket : Ticket.t option) (fixed_ticket_key: ticket_key) (inboxes: (nat, message list) big_map) (owner: address) (ticket: Ticket.t) : operation list * state =
  let (addr, (payload, quantity)), fresh_ticket = Ticket.read_ticket ticket in
  let opt_joined_ticket = 
    match state_ticket with
    | None -> 
      if is_same_ticket_key fixed_ticket_key (addr,payload) 
        then
          Some fresh_ticket
      else None
    | Some ticket ->
      Ticket.join_tickets ticket fresh_ticket in
  let joined_ticket =
    match opt_joined_ticket with
     | None -> failwith "Ticket key is invalid"
     | Some ticket -> ticket in
  let message = make_deposit_message owner quantity in
  let new_inboxes = push_message max_inbox_level inboxes message in
  let new_state = {  max_inbox_level = max_inbox_level ; ticket = (Some joined_ticket) ; fixed_ticket_key = fixed_ticket_key ; inboxes = new_inboxes} in
  ([], new_state)

let transaction (max_inbox_level: nat) (source : address) (destination: address) (quantity: nat) (fixed_ticket_key: ticket_key) (inboxes: inboxes) (ticket: Ticket.t option) : operation list * state =
  let message = make_transaction_message source destination quantity in
  let new_inboxes = push_message max_inbox_level inboxes message in
  let new_state = {  max_inbox_level = max_inbox_level; ticket = ticket ; fixed_ticket_key = fixed_ticket_key ; inboxes = new_inboxes} in
  ([], new_state)


let main (action, state : entrypoint * state) : operation list * state =
  let {max_inbox_level;ticket;fixed_ticket_key;inboxes} = state in
  match action with
  | Inbox_deposit ticketSent ->
    let ticketSent_owner = Tezos.sender in
    deposit max_inbox_level ticket fixed_ticket_key inboxes ticketSent_owner ticketSent
  | Inbox_transaction { destination; quantity; } ->
    let source = Tezos.sender in
    transaction max_inbox_level source destination quantity fixed_ticket_key inboxes ticket
