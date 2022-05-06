type chusai = bytes ticket
type mint_parameter =
      Mint 
    | Redeem of chusai
type wallet_parameter =
    Store of chusai
    | Go_mint of address
    | Go_redeem of address
    | Nope 

type storage = chusai option

(* ENDPOINTS *)
let main (action, store : wallet_parameter * storage) : operation list * storage = 
    match action with
        Store ticket -> [],(Some ticket)
        | Nope -> [],store
        | Go_mint addr -> 
            let mint_contr : mint_parameter contract = Tezos.get_contract_with_error addr "Mint Contract not found." in
            [Tezos.transaction Mint Tezos.amount mint_contr ],store
        | Go_redeem addr -> 
            let mint_contr : mint_parameter contract = Tezos.get_contract_with_error addr "Mint Contract not found." in
            (match store with
                | None -> failwith "No ticket to redeem"
                | Some t -> 
                    [Tezos.transaction (Redeem t) 0tz mint_contr ],None)
    