#import "../../commons/inbox_interface.mligo" "Inbox"
#import "../../commons/transaction.mligo" "Tx"
#import "../../commons/ticket/chusai_ticket.mligo" "Ticket"
#import "../../chain/src/chain_endpoints.mligo" "Chain"
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
; chain: Chain.chain
}

let make_deposit_message (owner: address) (quantity: nat) : message =
  Deposit { owner = owner; quantity = quantity }

let make_transaction_message (source: address) (destination: address) (quantity: nat) : message =
  Transaction { source = source ; destination = destination; quantity = quantity; arg = (None : bytes option) }

let make_freeze_message (owner: address) (quantity: nat) : message =
  Freeze { owner = owner; quantity = quantity }

(** When a message is push into an inbox, [current_inbox_level]
    will be bumped and is used as index of the inbox. One inbox
    only includes one message.

    [push_message] will return new [inbox_level] and new
    [inboxes].*)
let rec push_message (current_inbox_level : nat) (current_inboxes : inboxes) (message: message) : inbox_level * inboxes =
  let next_inbox_level = current_inbox_level + 1n in
  match Big_map.find_opt next_inbox_level current_inboxes with
  | None -> (next_inbox_level, Big_map.add next_inbox_level [ message ] current_inboxes)
  | Some _ -> push_message next_inbox_level current_inboxes message

let is_same_ticket_key (k1 : ticket_key) (mint_address, payload : address * Ticket.payload) : bool =
  k1.mint_address = mint_address && k1.payload = payload

let deposit ({max_inbox_level;ticket;fixed_ticket_key;inboxes;chain} : state) (owner: address) (new_ticket: Ticket.t) : operation list * state =
  let (addr, (payload, quantity)), fresh_ticket = Ticket.read_ticket new_ticket in
  let opt_joined_ticket = 
    match ticket with
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
  let new_inbox_level, new_inboxes = push_message max_inbox_level inboxes message in
  let new_state = {  max_inbox_level = new_inbox_level; ticket = (Some joined_ticket) ; fixed_ticket_key = fixed_ticket_key ; inboxes = new_inboxes ; chain = chain} in
  ([], new_state)

let transaction ({max_inbox_level;ticket;fixed_ticket_key;inboxes;chain} : state)   (source : address) (destination: address) (quantity: nat) : operation list * state =
  let message = make_transaction_message source destination quantity in
  let new_inbox_level, new_inboxes = push_message max_inbox_level inboxes message in
  let new_state = {  max_inbox_level = new_inbox_level; ticket = ticket ; fixed_ticket_key = fixed_ticket_key ; inboxes = new_inboxes ; chain = chain} in
  ([], new_state)

let receive ({max_inbox_level;ticket;fixed_ticket_key;inboxes;chain} : state) (proposal : Chain.block_proposal) =
  let ops, new_chain = Chain.Endpoints.apply_receive (proposal,chain) in
  let new_state = {  max_inbox_level = max_inbox_level; ticket = ticket ; fixed_ticket_key = fixed_ticket_key ; inboxes = inboxes ; chain = new_chain} in
  ops, new_state

let remove_block ({max_inbox_level;ticket;fixed_ticket_key;inboxes;chain} : state) (index : Chain.index) =
  let ops, new_chain = Chain.Endpoints.apply_remove (index,chain)  in
  let new_state = {  max_inbox_level = max_inbox_level; ticket = ticket ; fixed_ticket_key = fixed_ticket_key ; inboxes = inboxes ; chain = new_chain} in
  ops, new_state

let finalize_block ({max_inbox_level;ticket;fixed_ticket_key;inboxes;chain} : state) =
  let ops, new_chain =  Chain.Endpoints.apply_finalize chain  in
  let new_state = {  max_inbox_level = max_inbox_level; ticket = ticket ; fixed_ticket_key = fixed_ticket_key ; inboxes = inboxes ; chain = new_chain} in
  ops, new_state

let withdraw ({max_inbox_level;ticket;fixed_ticket_key;inboxes;chain} : state) =
  let ops, new_chain =  Chain.Endpoints.apply_withdraw chain  in
  let new_state = {  max_inbox_level = max_inbox_level; ticket = ticket ; fixed_ticket_key = fixed_ticket_key ; inboxes = inboxes ; chain = new_chain} in
  ops, new_state

let freeze ({max_inbox_level;ticket;fixed_ticket_key;inboxes;chain} : state) (owner: address) (quantity: Chain.frozen_amount) : operation list * state=
  let message = make_freeze_message owner quantity in
  let new_inbox_level, new_inboxes = push_message max_inbox_level inboxes message in
  let new_state = {  max_inbox_level = new_inbox_level; ticket = ticket ; fixed_ticket_key = fixed_ticket_key ; inboxes = new_inboxes ; chain = chain} in
  ([], new_state)


let main (action, state : entrypoint * state) : operation list * state =
  match action with
  | Inbox_deposit ticketSent ->
    let ticketSent_owner = Tezos.sender in
    deposit state ticketSent_owner ticketSent
  | Inbox_transaction { destination; quantity; } ->
    let source = Tezos.sender in
    transaction state  source destination quantity 
  | Inbox_receive_block proposal -> 
    receive state proposal
  | Inbox_remove_block  index -> 
    remove_block state index
  | Inbox_finalize_block -> 
    finalize_block state
  | Inbox_freeze {quantity} -> 
    let user = Tezos.sender in
    freeze state user quantity
  | Inbox_withdraw ->
    withdraw state

[@view] let get_latest ((),state: unit * state) : Chain.block option = Chain.Views.get_latest ((), state.chain)
[@view] let get_next_finalization_candidate ((), state : unit * state) : Chain.block option = Chain.Views.get_next_finalization_candidate ((), state.chain)