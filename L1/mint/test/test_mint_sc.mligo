#include "../src/mint_sc.mligo"

let debug = false 

let main_test (tests: unit -> unit) (test_redeem : chusai -> chusai) (action, _store : mint_parameter * unit) : operation list * unit = 
    (match action with
          Mint  -> 
          begin
            tests ();
            assert (Tezos.amount > 0tez);
            mint ()
          end
        | Redeem ticket -> 
          begin
            let ticket:chusai = test_redeem ticket in
            redeem ticket
          end)
    , ()

let tests_function_axample () = ()
let test_redeem_function (ticket:chusai) = ticket
let mint_test_taddr, _, _ = Test.originate (main_test tests_function_axample test_redeem_function) () 0tez

let _u = Test.reset_state 5n ([] : tez list)
let baker1 = Test.nth_bootstrap_account(0)
let admin = Test.nth_bootstrap_account(1)

let _ = Test.set_source(admin)
let _ = Test.set_baker(baker1)

let mint_taddr, _, _ = Test.originate main () 0tez
let mint_contr = Test.to_contract mint_taddr
let mint_addr = Tezos.address mint_contr
let _ = Test.log ("Mint address",mint_addr)

(* Test Wallet *)
(* 
  a minimum test implementation of wallet contract
  used as a proxy to test the mint
*)
type wallet_storage = chusai option
type wallet_asserts = {payload : chusai_payload}
let wallet_test_main (src : address) (action, store : wallet_parameter * wallet_storage) : operation list * wallet_storage = 
    match action with
        Store ticket ->         
          let (addr, (payload, total)),ticket = Tezos.read_ticket ticket in
          begin
            assert (addr = mint_addr);
            assert_with_error (total > 0n) "wrong value of ticket";
            [],(Some ticket)
          end
        | Nope -> 
          let src_contr : unit contract = Tezos.get_contract_with_error src "No src" in
          [],store
        | Go_mint _addr -> [Tezos.transaction Mint Tezos.amount mint_contr ],store
        | Go_redeem _addr -> 
            let ticket = Option.unopt store in
            [Tezos.transaction (Redeem ticket) 0tz mint_contr ],None

type wallet = 
  {
    taddr : (wallet_parameter , wallet_storage) typed_address ; 
    contr : wallet_parameter contract ; 
    addr : address
  }

let originate_wallet () = 
let wallet_taddr, _, _ = Test.originate (wallet_test_main admin)(None : wallet_storage) 0tez in
let wallet_contr = Test.to_contract wallet_taddr in
let wallet_addr = Tezos.address wallet_contr in
let _ = Test.log ("Wallet address",wallet_addr) in
{taddr = wallet_taddr ; contr = wallet_contr ; addr = wallet_addr}


let test_coucou = 
  let wallet = originate_wallet () in
  let _gas_cons = Test.transfer_to_contract_exn wallet.contr (Go_mint mint_addr) 100tz in
  let _balance = assert ((Test.get_balance wallet.addr) = 0tz) in 
  let _gas_cons = Test.transfer_to_contract_exn wallet.contr (Go_redeem mint_addr) 0tz in
  let ticket_opt : wallet_storage= Test.get_storage wallet.taddr in
  begin
  assert (match ticket_opt with
    | None -> true
    | Some _ -> false);
  assert ((Test.get_balance wallet.addr) = 85tz)
  end
type test_param = {src : address ; amount_ : tez ; mint:address}
let mint_then_redeem (param,wallet:test_param*wallet) =
  let _gas_cons = Test.transfer_to_contract_exn wallet.contr (Go_mint param.mint) param.amount_ in
  let _balance = assert ((Test.get_balance wallet.addr) = 0tz) in 
  let _gas_cons = Test.transfer_to_contract_exn wallet.contr (Go_redeem param.mint) 0tz in
  let ticket_opt : wallet_storage= Test.get_storage wallet.taddr in
  assert (match ticket_opt with
    | None -> true
    | Some _ -> false)

let test_coucou2 = 
  begin
  let wallet = originate_wallet () in
    mint_then_redeem ({src = admin; amount_ = 100tz ; mint = mint_addr},wallet);
  assert ((Test.get_balance wallet.addr) = 85tz)

  end
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