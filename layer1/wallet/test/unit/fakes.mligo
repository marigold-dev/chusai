#include "utils.mligo"

let fake_mint_main (action, store : mint_parameter * unit) : operation list * unit =
  match action with
  | Mint callback_entrypoint ->
    let ticket = create_ticket dummy_payload (Tezos.amount / 1tez) in
    let op = Tezos.transaction ticket 0tez callback_entrypoint in 
    ([op], unit)
  | Redeem (ticket, callback_entrypoint) ->
      let (_, (_, ticket_value)), _ticket2 = read_ticket ticket in
      let op = Tezos.transaction unit (ticket_value * 1tez) callback_entrypoint in 
      ([op], unit)

let fake_bridge_main (parameter, storage: bridge_parameter * bridge_storage) : bridge_return = 
  match parameter with
    | Deposit ticket_to_deposit ->
      let new_tickets = match storage.tickets with 
        | None -> Some ticket_to_deposit
        | Some existing_ticket -> join_tickets ticket_to_deposit existing_ticket in
      let new_storage = {
        tickets = new_tickets
      } in 
      ([], new_storage)