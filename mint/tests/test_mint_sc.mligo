#include "../src/mint_sc.mligo"

let _u = Test.reset_state 5n ([] : tez list)
let debug = false 



let _u = Test.reset_state 5n ([] : tez list)
let baker1 = Test.nth_bootstrap_account(0)
let admin = Test.nth_bootstrap_account(1)

let _ = Test.set_source(admin)
let _ = Test.set_baker(baker1)

let mint_taddr, _, _ = Test.originate main () 0tez
let mint_contr = Test.to_contract mint_taddr
let mint_addr = Tezos.address mint_contr
let _ = Test.log ("Mint address",mint_addr)

type wallet_storage = chusai option
let wallet_test_main (action, store : wallet_parameter * wallet_storage) : operation list * wallet_storage = 
    match action with
        Store ticket -> [],(Some ticket)
        | Nope -> [],store
        | Go_mint _addr -> 
            (* let mint_contr : mint_parameter contract = Tezos.get_contract_with_error addr "Mint Contract not found." in *)
            [Tezos.transaction Mint Tezos.amount mint_contr ],store
        | Go_redeem _addr -> 
            (* let mint_contr : mint_parameter contract = Tezos.get_contract_with_error addr "Mint Contract not found." in *)
            (match store with
                | None -> failwith "No ticket to redeem"
                | Some t -> 
                    [Tezos.transaction (Redeem t) 0tz mint_contr ],None)
                    
let wallet_taddr, _, _ = Test.originate wallet_test_main (None : wallet_storage) 0tez
let wallet_contr = Test.to_contract wallet_taddr
let wallet_addr = Tezos.address wallet_contr
let _ = Test.log ("Wallet address",wallet_addr)

let test_coucou = 
  let _gas_cons = Test.transfer_to_contract_exn wallet_contr (Go_mint mint_addr) 100tz in
  let _balance = assert ((Test.get_balance wallet_addr) = 0tz) in 
  let _gas_cons = Test.transfer_to_contract_exn wallet_contr (Go_redeem mint_addr) 0tz in
  let ticket_opt : wallet_storage= Test.get_storage wallet_taddr in
  let _no_ticket = assert (match ticket_opt with
    | None -> true
    | Some _ -> false) in
  let _balance = assert ((Test.get_balance wallet_addr) = 85tz) in 
  true

(* let test_balance_after_deposit = 
  let () = if debug then Test.log "test_balance_after_deposit" in
  let mint_taddr, _, _ = Test.originate main () 0tez in
  let wallet_addr, program, size = Test.originate_from_file wallet_file "main" ([] : string list) (Test.eval unit) 1000tez in
  let () = if debug then Test.log ("contract address", Tezos.address (Test.to_contract mint_taddr)) in
  let contr = Test.to_contract mint_taddr in
  let deposit = 1000tez in
  let _set_source = Test.set_source wallet_addr in 
  let _gas_cons = Test.transfer_to_contract_exn contr Mint deposit in
  // the contract's balance is increased by "deposit"
  assert ( Test.get_balance (Tezos.address contr) = deposit)  *)

(* 
let test_balance_after_deposit = 
  let mint_taddr, _, _ = Test.originate main () 0tez in
  let wallet_addr, program, size = Test.originate_from_file wallet_file "main" ([] : string list) (Test.eval unit) 1000tez in
  let _set_source = Test.set_source wallet_addr in 
  let _gas_cons = Test.transfer_to_contract_exn (Test.to_contract mint_taddr) Mint 1000tez in
  assert ( Test.get_balance (Tezos.address (Test.to_contract mint_taddr)) = 1000tez)  *)