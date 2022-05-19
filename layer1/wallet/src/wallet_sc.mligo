#include "../../common/common.mligo"
#include "../../stdlib_ext/src/stdlibext.mligo"

let contract_name : string = "wallet_sc"
let error_message (msg : string) : string = String.concat contract_name (String.concat ":" msg)
let throw_error (type a) (x : string) : a = failwith (error_message x)

(*
Responsibility: Sends tez to the mint contract in order to obtain a ticket
*)
let mint_xtz ({mint_address; bridge_address; ticket_storage} : wallet_storage) : wallet_return =
  let ticket_value = Tezos.amount in
  let _ = if ticket_value = 0tez then throw_error "Amount should be non-zero" in
  let callback_entrypoint = (Tezos.self "%mint_xtz_cb" : chusai_ticket contract) in
  let mint_contract : mint_parameter contract = Tezos.get_contract_with_error mint_address (error_message "Mint smart contract address doesn't exist") in
  let ops = [Tezos.transaction (Mint callback_entrypoint) ticket_value mint_contract] in
  let new_storage = {
      mint_address = mint_address; 
      bridge_address = bridge_address; 
      ticket_storage = ticket_storage 
    } in
  (ops, new_storage)

(*
Responsibility: Stores the incoming ticket in to the contract storage 
by joining with the existing ticket, if needed
*)
let mint_xtz_cb (ticket_to_add, {mint_address; bridge_address; ticket_storage} : chusai_ticket*wallet_storage) : wallet_return = 
  let join_or_fail (existing_ticket : chusai_ticket) =  
    let joined_tickets = join_tickets existing_ticket ticket_to_add in
    let _ =  if (OptionExt.is_none joined_tickets) then throw_error "Ticket payload is invalid" in
    joined_tickets in 
  let joined_tickets : chusai_ticket option = OptionExt.bind ticket_storage join_or_fail in

  let new_ticket_storage : chusai_ticket option = OptionExt.or_else joined_tickets (Some ticket_to_add) in
  let new_storage = {
    mint_address = mint_address;
    bridge_address = bridge_address;
    ticket_storage = new_ticket_storage
  } in
  ([], new_storage) 
  
let redeem_xtz ({mint_address; bridge_address; ticket_storage} : wallet_storage) : wallet_return =
  let ticket = Option.unopt_with_error ticket_storage (error_message "No ticket found in storage") in
  let callback_entrypoint = (Tezos.self "%redeem_xtz_cb" : unit contract) in
  let mint_contract : mint_parameter contract = Tezos.get_contract_with_error mint_address (error_message "Mint smart contract address doesn't exist") in
  let ops = [Tezos.transaction (Redeem (ticket, callback_entrypoint)) 0tez mint_contract] in
  let new_storage = {
      mint_address = mint_address; 
      bridge_address = bridge_address; 
      ticket_storage = (None : chusai_ticket option)
    } in
  (ops, new_storage) 

let redeem_xtz_cb (storage: wallet_storage) : wallet_return =
  ([], storage)
  
let send ({mint_address; bridge_address; ticket_storage} : wallet_storage) : wallet_return =
  let ticket = Option.unopt_with_error ticket_storage (error_message "No ticket found in storage") in
  let bridge_contract : bridge_parameter contract = Tezos.get_contract_with_error bridge_address (error_message "Bridge smart contract address doesn't exist") in
  let ops = [Tezos.transaction (Deposit ticket) 0tez bridge_contract] in
  let new_storage = {
      mint_address = mint_address; 
      bridge_address = bridge_address; 
      ticket_storage = (None : chusai_ticket option)
    } in
  (ops, new_storage)

(*  
Rsponsibility: dispatch based on action to the action handler functions
 *)
let main (parameter, storage : wallet_parameter * wallet_storage) : wallet_return =
  match parameter with
    | Mint_xtz -> mint_xtz storage 
    | Mint_xtz_cb ticket -> mint_xtz_cb (ticket,storage)
    | Send -> send storage
    | Redeem_xtz -> redeem_xtz storage
    | Redeem_xtz_cb -> redeem_xtz_cb storage