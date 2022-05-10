type my_ticket = string ticket
type mint_parameter =
      Mint 
    | Redeem of my_ticket
type wallet_parameter =
      Store of my_ticket
    | Answer of string
    | Go_mint of address
    | Go_redeem of address


(* REDEEMING *)
let redeem (ticket:my_ticket) : operation list = 
    let (_addr, (payload, _total)), _ticket = Tezos.read_ticket ticket in
    let op = Tezos.transaction (Answer payload) 0tez ((Tezos.get_contract_with_error (Tezos.sender) "Wallet Answer contract not found."):wallet_parameter contract) in
    [op]

(* MINTING *)
let mint () : operation list = 
    let ticket = Tezos.create_ticket "Hello" 1n in
    let op = Tezos.transaction (Store ticket) 0tez ((Tezos.get_contract_with_error (Tezos.sender) "Wallet Store contract not found"):wallet_parameter contract) in
    [op]

(* ENDPOINTS *)
let main (action, _store : mint_parameter * unit) : operation list * unit = 
    (match action with
          Mint  -> mint ()
        | Redeem ticket -> redeem ticket)
    , ()