#include "../src/mint_sc.mligo"
#include "test_utils.mligo"

let main_test (test_mint: unit -> unit) (test_redeem : chusai -> chusai) (action, _store : mint_parameter * unit) : operation list * unit = 
    (match action with
          Mint  -> 
          begin
            test_mint ();
            mint ()
          end
        | Redeem ticket -> 
          begin
            let ticket:chusai = test_redeem ticket in
            redeem ticket
          end)
    , ()


let _u = Test.reset_state 5n ([] : tez list)
let baker1 = Test.nth_bootstrap_account(0)
let admin = Test.nth_bootstrap_account(1)

let _ = Test.set_source(admin)
let _ = Test.set_baker(baker1)


(* *********************************** *)
(* Test Wallet *)
(* 
  a minimum test implementation of wallet contract
  used as a proxy to test the mint
*)
type wallet_storage = chusai option
let wallet_test_main 
  (check_ticket: chusai -> chusai) 
  (action, store : wallet_parameter * wallet_storage) 
    : operation list * wallet_storage = 
    match action with
        Store ticket ->         
          let (addr, (payload, total)),ticket = Tezos.read_ticket ticket in
          begin
            let ticket = check_ticket ticket in
            [],(Some ticket)
          end
        | Nope -> 
          [],store
        | Go_mint addr -> 
            let mint_contr : mint_parameter contract= Tezos.get_contract_with_error addr "No mint to mint" in
            [Tezos.transaction Mint Tezos.amount mint_contr ],store
        | Go_redeem addr -> 
            let ticket = Option.unopt store in
            let mint_contr : mint_parameter contract = Tezos.get_contract_with_error addr "No mint to redeem" in
            [Tezos.transaction (Redeem ticket) 0tz mint_contr ],None


(* *********************************** *)
(* Origination wrappers  *)
type originated_wallet = (wallet_parameter,wallet_storage) originated
let originate_wallet (check_ticket: chusai -> chusai) : originated_wallet = 
    originate_full 
      ((wallet_test_main check_ticket )
      ,(None : wallet_storage)
      ,0tez
      , "Wallet contract")

type originated_mint = (mint_parameter,unit) originated
let originate_mint () : originated_mint = originate_full (main,(),0tez, "Mint contract")



(* *********************************** *)
(* Simplified calls to Mint and Redeem *)
type mint_param = 
{
  wallet : originated_wallet;
  amount_ : tez ; 
  mint : address 
}

let mint_ (param  : mint_param) (previous:test_exec_result)=
  transfer_to_contract param.wallet.contr (Go_mint param.mint) param.amount_ previous 

let redeem_ (param  : mint_param) (previous:test_exec_result)=
  transfer_to_contract param.wallet.contr (Go_redeem param.mint) 0tz previous 


(* *********************************** *)
(* TESTS *)


(* simple check of initial storage/balance *)
let test_mint_origination = 
  begin
  log_ "test_mint_origination";
  let mint = originate_mint () in
  assert_ ((Test.get_balance mint.addr) = 0tz) "balance should be 0" init_result 
  end

(* we sent an amount to mint
   - we got a ticket with correct mint, payload and amount
   - balance of mint is ok
*)
let test_mint_first_ticket = 
  begin
  log_ "test_mint_first_ticket";
  let mint = originate_mint () in
  (* we check in wallet the tickets upon reception *)
  let ticket_asserts : ticket_asserts= {addr = Some mint.addr ; payload = Some chusai_payload; amount_ = Some 1000000n } in
  let wallet = originate_wallet (check_ticket ticket_asserts) in
  (* minting *)
  let result = mint_  ({wallet = wallet; amount_ = 1tz ; mint = mint.addr  } ) init_result in
  (* asserts *)
  let result = assert_ ((Test.get_balance wallet.addr) = 0tz) "wallet balance should be 0" result in 
  assert_ ((Test.get_balance mint.addr) = 1tz) "mint balance should be 1tez" init_result 
  end

(* we sent an inappropriate amount 
    - no ticket received
    - mint is not credited 
*)
let test_mint_first_ticket_mutez = 
  begin
  log_ "test_mint_first_ticket_mutez";
  let mint = originate_mint () in
  let wallet = originate_wallet (fun (t:chusai) -> t) in
  (* minting *)
  let result = mint_  ({wallet = wallet; amount_ = 1mutez ; mint = mint.addr  } ) init_result in
  (* asserts *)
  let result = assert_rejected_at mint.addr "should be rejected 'cause less than minimum amount" result in
  let result = assert_none_ (Test.get_storage wallet.taddr ) "there should be no ticket received" result in  
  assert_ ((Test.get_balance mint.addr) = 0tz) "balance should be 0 cause no ticket provided" result  
  end

(* we sent an inappropriate amount 
    - no ticket received
    - mint is not credited 
*)
let test_mint_first_ticket_0tez = 
  begin
  log_ "test_mint_first_ticket_0tez";
  let mint = originate_mint () in
  let wallet = originate_wallet (fun (t:chusai) -> t) in
  (* minting *)
  let result = mint_  ({wallet = wallet; amount_ = 0mutez ; mint = mint.addr  } ) init_result in
  (* asserts *)
  let result = assert_rejected_at mint.addr "should be rejected 'cause less than minimum amount" result in
  let result = assert_none_ (Test.get_storage wallet.taddr ) "there should be no ticket received" result in  
  assert_ ((Test.get_balance mint.addr) = 0tz) "balance should be 0 cause no ticket provided" result  
  end

(* we mint and redeem at same Mint
  - balances should change
  - taxes should be deduced
  - ticket should be burned after redeeming
*)
let test_mint_and_redeem = 
  begin
  log_ "test_mint_and_redeem";
  let mint = originate_mint () in
  let ticket_asserts : ticket_asserts= {addr = Some mint.addr ; payload = Some chusai_payload; amount_ = Some 100000000n } in
  let wallet = originate_wallet (check_ticket ticket_asserts) in
  (* minting *)
  let result = mint_  ({wallet = wallet; amount_ = 100tz ; mint = mint.addr  } ) init_result in
  let result = assert_is_ok "sanity check : Minting should have succeeded" result in 
  let result = assert_ ((Test.get_balance wallet.addr) = 0tz) "sanity check : wallet balance should be 0" result in 
  (* redeeming *)
  let result = redeem_  ({wallet = wallet; amount_ = 0tz ; mint = mint.addr  } ) result in
  (* asserts *)
  let result = assert_none_ (Test.get_storage wallet.taddr ) "there should be no ticket left in wallet storage" result in  
  let result = assert_ ((Test.get_balance mint.addr) > 0tz) "mint should have retained taxes" result in 
  assert_ ((Test.get_balance wallet.addr) = 85tz) "taxes should have been deduced" result 
  end


(* we mint at 1 mint and redeem at another
  - should fail
  - no balance modification
*)
let test_redeem_at_wrong_mint = 
  begin
  log_ "test_redeem_at_wrong_mint";
  let mint_1 = originate_mint () in
  let mint_2 = originate_mint () in
  let ticket_asserts : ticket_asserts = {no_assert with addr = Some mint_1.addr  } in
  let wallet = originate_wallet (check_ticket ticket_asserts) in  
  (* minting *)
  let result = mint_  ({wallet = wallet; amount_ = 100tz ; mint = mint_1.addr  } ) init_result in
  let result = assert_is_ok "sanity check : Minting should have succeeded" result in 
  let result = assert_ ((Test.get_balance wallet.addr) = 0tz) "sanity check : balance should be 0" result in 
  (* redeeming *)
  let result = redeem_  ({wallet = wallet; amount_ = 0tz ; mint = mint_2.addr  } ) result in
  (* asserts *)
  let result = assert_ ((Test.get_balance wallet.addr) = 0tz) "balance of wallet should be 0" result in 
  let result = assert_ ((Test.get_balance mint_1.addr) = 100tz) "balance of mint 1 should be 100tez" result in 
  let result = assert_ ((Test.get_balance mint_2.addr) = 0tz) "balance of mint 2 should be 0tez" result in 
  assert_rejected_at mint_2.addr "should be rejected by second mint" result 
  end 
