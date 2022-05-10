type my_ticket = string ticket
type mint_parameter =
      Mint 
    | Redeem of my_ticket
type wallet_parameter =
      Store of my_ticket
    | Answer of string
    | Go_mint of address
    | Go_redeem of address

type storage = {
    msg : string;
    the_ticket : my_ticket option
}

let mint (addr:address) = 
    Tezos.transaction (Mint) 0tez ((Tezos.get_contract_with_error addr "Contract not found.") : mint_parameter contract)
    
let redeem (addr,ticket_opt : address * (my_ticket option)) : operation= 
    match ticket_opt with
        | None -> failwith "no ticket to redeem"
        | Some t -> Tezos.transaction (Redeem t) 0tez ((Tezos.get_contract_with_error addr "Contract not found.") : mint_parameter contract)

(* ENDPOINTS *)
let main (action, store : wallet_parameter * storage) : operation list * storage = 
    let {msg ; the_ticket} = store in
    match action with
          // stores the ticket (lose previous if any)
          Store ticket -> [] , {msg = msg; the_ticket = (Some ticket)}
          // stores msg (lose previous if any)
        | Answer str ->  [] , {msg = str; the_ticket = the_ticket}
          // contact Mint at provided address (keeps storage)
        | Go_mint mint_addr ->  [mint mint_addr] , {msg = msg; the_ticket = the_ticket}
          // contact Redeem at provided address (transfer ticket)
        | Go_redeem mint_addr -> [redeem (mint_addr,the_ticket)] , {msg = msg; the_ticket = None}
    