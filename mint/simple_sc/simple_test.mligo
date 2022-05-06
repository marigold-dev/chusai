#include "../sc/simple_mint.mligo"
#import "../sc/simple_wallet.mligo" "WALLET"

let _u = Test.reset_state 5n ([] : tez list)
let baker1 = Test.nth_bootstrap_account(0)
let admin = Test.nth_bootstrap_account(1)

let _ = Test.set_source(admin)
let _ = Test.set_baker(baker1)

let initial_wallet : WALLET.storage = {msg = ""; the_ticket = None}

let mint_taddr, _, _ = Test.originate main () 0tez
let mint_contr = Test.to_contract mint_taddr
let mint_addr = Tezos.address mint_contr
let _ = Test.log ("Mint address",mint_addr)

let wallet_taddr, _, _ = Test.originate WALLET.main initial_wallet 0tez
let wallet_contr = Test.to_contract wallet_taddr
let wallet_addr = Tezos.address wallet_contr
let _ = Test.log ("Wallet address",wallet_addr)

let test_coucou = 
  let _gas_cons = Test.transfer_to_contract_exn wallet_contr (Go_mint mint_addr) 0tz in
  let _gas_cons = Test.transfer_to_contract_exn wallet_contr (Go_redeem mint_addr) 0tz in
  let {msg ; the_ticket} = Test.get_storage wallet_taddr in
  msg = "Hello"