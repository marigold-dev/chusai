type chusai = bytes ticket
type mint_parameter =
      Mint 
    | Redeem of chusai
type wallet_parameter =
    Store of chusai
    | Go_mint of address
    | Go_redeem of address
    | Nope

(* CONFIGURATION *)
let tax_rate = 15n // in %
let chusai_payload = 0x7070
let chusai_ticketer () = Tezos.self_address

(* XTZ <-> CHUSAI*)
let minimum_amount = 1tez
let xtz_to_chusai (xtz:tez) = xtz /1mutez
let chusai_to_xtz (chusai:nat) = 
    let n = (chusai * abs(100n - tax_rate)) / 100n in
    n * 1mutez 

let test_conversion_mutez = assert ((xtz_to_chusai 1mutez) = 1n)
let test_conversion_tez = assert ((xtz_to_chusai 1tez) = 1000000n)
let test_conversion_42tez = assert ((xtz_to_chusai 42tez) = 42000000n)
let test_conversion_chusai_tez = assert ((chusai_to_xtz 1000000n) = 850000mutez)
let test_conversion_chusai_100mutez = assert ((chusai_to_xtz 100n) = 85mutez)
let test_conversion_chusai_1mutez = assert ((chusai_to_xtz 1n) = 0mutez)

(* MINTING *)
let create_chusai () : chusai =
    let n : nat = xtz_to_chusai Tezos.amount  in
    Tezos.create_ticket chusai_payload n

let transaction_of_mint (ticket:chusai) =
    let sender_contract : wallet_parameter contract =
      Tezos.get_contract_with_error (Tezos.sender) "Contract not found. Cannot send ticket" in
    Tezos.transaction (Store ticket) 0tez sender_contract

let mint () : operation list = 
    let _check = if( Tezos.amount < minimum_amount) then failwith "no ticket for less than ..." in    
    let ticket = create_chusai () in
    let op = transaction_of_mint ticket in
    [op]

(* REDEEMING *)
let transaction_of_redeem (requested:nat)  = 
    let sender_contract : wallet_parameter contract =
      Tezos.get_contract_with_error (Tezos.sender) "Contract not found. Cannot redeem" in
    Tezos.transaction Nope (chusai_to_xtz requested ) sender_contract

let redeem (ticket:chusai) : operation list = 
    let (addr, (payload, total)), _ticket = Tezos.read_ticket ticket in
    let _check = if( addr <> (chusai_ticketer ())) then failwith "wrong ticketer" in   
    let _check = if( payload <> chusai_payload) then failwith "wrong payload" in
    let op = transaction_of_redeem total in
    [op]

(* ENDPOINTS *)
let main (action, _store : mint_parameter * unit) : operation list * unit = 
    (match action with
          Mint  -> mint ()
        | Redeem ticket -> redeem ticket)
    , ()
