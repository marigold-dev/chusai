#include "../src/mint_sc.mligo"
#include "test_utils.mligo"
#import "../../stdlib_ext/src/atomic_test.mligo" "Atom"
#import "../../stdlib_ext/src/stdlibext.mligo" "Stdlib"

let _u = Test.reset_state 5n ([] : tez list)


(* *********************************** *)
(* Test Wallet *)
(* 
  a minimum test implementation of wallet contract
  used as a proxy to test the mint
  doesn't join tickets or anything fancy

  takes a function "check_ticket" that allows injection of operation on the ticket received.
  Note : at the moment the Test module doesn't allow get_storage in the presence of tickets, 
    the check_ticket function can be used to go around the pb

  To obtain a main function that can be originated, use currying
*)
type wallet_storage = chusai_ticket option
type wallet_parameter =
      Store of chusai_ticket // callback to receive ticket
    | Go_mint of address   // to start minting process from implicit account
    | Go_redeem of address // to start redeem process from implicit account
    | Nope                 // callback to receive money
let wallet_test_main 
  (check_ticket : chusai_ticket-> chusai_ticket) 
  (action, store : wallet_parameter * wallet_storage) 
    : operation list * wallet_storage = 
    match action with
    | Store ticket ->         
        let (addr, (payload, total)), ticket = read_ticket ticket in
        begin
          let ticket = check_ticket ticket in
          [], (Some ticket)
        end
    | Nope -> 
      [], store
    | Go_mint addr -> 
        let mint_contr : mint_parameter contract= Tezos.get_contract_with_error addr "No mint to mint" in
        let mint_cb : chusai_ticket contract = Tezos.self "%store" in
        [Tezos.transaction (Mint mint_cb) Tezos.amount mint_contr], store
    | Go_redeem addr -> 
        let ticket = Option.unopt store in
        let mint_contr : mint_parameter contract = Tezos.get_contract_with_error addr "No mint to redeem" in
        let redeem_cb : unit contract = Tezos.self "%nope" in            
        [Tezos.transaction (Redeem (ticket, redeem_cb)) 0tz mint_contr], None


(* *********************************** *)
(* Origination wrappers  *)
(*
  Based on originate_full function in test utils. 
*)
type originated_wallet = (wallet_parameter, wallet_storage) originated
(* originates a wallet, given a function to be applied on the ticket when receiving one *)
let originate_wallet (injected_logic : chusai_ticket-> chusai_ticket) : originated_wallet = 
    originate_full 
      ((wallet_test_main injected_logic)
      , (None : wallet_storage)
      , 0tez
      , "Wallet contract")

type originated_mint = (mint_parameter, storage) originated
let mint_default_storage = {payload = 0x00 ; minimum_amount = 1tez}
(* originates a mint *)
let originate_mint (store : storage) : originated_mint = originate_full (main, store, 0tez, "Mint contract")



(* *********************************** *)
(* Simplified calls to Mint and Redeem *)
type mint_param = 
{
  wallet : originated_wallet;
  amount_ : tez ; //0tz when redeeming (not used)
  mint : address 
}

let mint_ (param : mint_param) (previous : Atom.status)=
  Atom.transfer_to_contract previous param.wallet.contr (Go_mint param.mint) param.amount_ 

let redeem_ (param : mint_param) (previous : Atom.status)=
  Atom.transfer_to_contract previous param.wallet.contr (Go_redeem param.mint) 0tz  


(* *********************************** *)
(* TESTS *)

(* Tests of pure functions *)
let _test_inline_conversion_mutez  () =  Atom.assert_ ((xtz_to_chusai_amount 1mutez) = 1n) "1 ticket per mutez"

let _test_inline_conversion_42tez  () =  Atom.assert_ ((xtz_to_chusai_amount 42tez) = 42000000n) "1 ticket per mutez"

let _test_inline_conversion_chusai_tez  () =  Atom.assert_ ((chusai_amount_to_xtz 1000000n) = 1tez) "1 mutez per ticket" 

let _test_inline_conversion_chusai_1mutez  () =  Atom.assert_ ((chusai_amount_to_xtz 1n) = 1mutez) "1 mutez per ticket"

(* simple check of initial storage/balance *)
let _test_mint_origination  () = 
  begin
    log_ "test_mint_origination";
    let mint = originate_mint mint_default_storage in
    Atom.assert_ ((Test.get_balance mint.addr) = 0tz) "balance should be 0"  
  end

(* we sent an amount to mint
   - we got a ticket with correct mint, payload and amount
   - balance of mint is ok
*)
let _test_mint_first_ticket  () = 
  begin
    log_ "test_mint_first_ticket";
    let mint = originate_mint mint_default_storage in
    (* we check in wallet the tickets upon reception *)
    let ticket_asserts : ticket_asserts = {addr = Some mint.addr ; payload = Some mint_default_storage.payload ; amount_ = Some 1000000n } in
    let wallet = originate_wallet (check_ticket ticket_asserts) in
    (* minting *)
    let status = mint_  {wallet = wallet; amount_ = 1tz ; mint = mint.addr}  Atom.start in
    (* asserts *)
    Atom.and_list 
    [  Atom.assert_is_ok status "Mint transaction should be OK" 
    ;  Atom.assert_ ((Test.get_balance wallet.addr) = 0tz) "wallet balance should be 0" 
    ;  Atom.assert_ ((Test.get_balance mint.addr) = 1tz) "mint balance should be 1tez" 
    ] 
  end

(* we sent an inappropriate amount (less than minimum)
    - no ticket received
    - mint is not credited 
*)
let _test_mint_first_ticket_mutez () = 
  begin
    log_ "test_mint_first_ticket_mutez";
    let mint = originate_mint {payload = 0x00 ; minimum_amount = 100tez} in // fixes minimum amount at 100tea
    let wallet = originate_wallet (fun (t : chusai_ticket) -> t) in
    (* minting *)
    let status = mint_  {wallet = wallet ; amount_ = 1tez ; mint = mint.addr}  Atom.start in // 1tez is less than minimum (100tez)
    (* asserts *)
    Atom.and_list 
    [  Atom.assert_rejected_at status mint.addr "should be rejected 'cause less than minimum amount"  
    ;  Atom.assert_ (Stdlib.OptionExt.is_none (Test.get_storage wallet.taddr)) "there should be no ticket received"  
    ;  Atom.assert_ ((Test.get_balance mint.addr) = 0tz) "balance should be 0 cause no ticket provided"   
    ]
  end

(* we sent an inappropriate amount (0)
    - no ticket received
    - mint is not credited 
*)
let _test_mint_first_ticket_0tez () = 
  begin
    log_ "test_mint_first_ticket_0tez";
    let mint = originate_mint mint_default_storage in
    let wallet = originate_wallet (fun (t : chusai_ticket) -> t) in
    (* minting *)
    let status = mint_  {wallet = wallet ; amount_ = 0mutez ; mint = mint.addr}  Atom.start in
    (* asserts *)
    Atom.and_list 
    [  Atom.assert_rejected_at status mint.addr "should be rejected 'cause less than minimum amount" 
    ;  Atom.assert_ (Stdlib.OptionExt.is_none (Test.get_storage wallet.taddr )) "there should be no ticket received" 
    ;  Atom.assert_ ((Test.get_balance mint.addr) = 0tz) "balance should be 0 cause no ticket provided"   
    ]
  end

(* we mint and redeem at same Mint
  - balances should change
  - taxes should be deduced
  - ticket should be burned after redeeming
*)
let _test_mint_and_redeem () = 
  begin
    log_ "test_mint_and_redeem";
    let mint = originate_mint mint_default_storage in
    (* the wallet will check the ticket on reception*)
    let ticket_asserts : ticket_asserts= {addr = Some mint.addr ; payload = Some mint_default_storage.payload ; amount_ = Some 100000000n} in
    let wallet = originate_wallet (check_ticket ticket_asserts) in
    (* minting *)
    let status = mint_ {wallet = wallet ; amount_ = 100tz ; mint = mint.addr}  Atom.start in
    let status = 
      Atom.and 
        (Atom.assert_is_ok status "sanity check : Minting should have succeeded") 
        (Atom.assert_  ((Test.get_balance wallet.addr) = 0tz) "sanity check : wallet balance should be 0")  in 
    (* redeeming *)
    let status = redeem_  {wallet = wallet ; amount_ = 0tz ; mint = mint.addr}  status in
    (* asserts *)
    Atom.and_list 
    [  Atom.assert_is_ok status "Redeem transaction should be OK" 
    ;  Atom.assert_  (Stdlib.OptionExt.is_none (Test.get_storage wallet.taddr)) "there should be no ticket left in wallet storage" 
    ;  Atom.assert_  ((Test.get_balance mint.addr) = 0tz) "mint should have nothing left" 
    ;  Atom.assert_  ((Test.get_balance wallet.addr) = 100tz) "wallet should have gotten xtz back"
    ]
  end

(* we try to redeem a 0-value ticket
  - mint should refuse to redeem
*)
(* function used to split ticket in wallet after minting, will produce a 0-value ticket *)
let turn_into_0value (ticket : chusai_ticket) : chusai_ticket = 
  let (_, (_, amount_)), ticket = read_ticket ticket in
  let opt : (chusai_ticket*chusai_ticket) option= split_ticket ticket (0n, amount_)  in
  let ticket0, _ticket = Option.unopt(opt) in
  ticket0
(* test *)
let _test_redeem_0value_ticket () = 
  begin
    log_ "test_redeem_0value_ticket";
    let mint = originate_mint mint_default_storage in
    (* the wallet will check the ticket on reception*)
    let ticket_asserts : ticket_asserts= {addr = Some mint.addr ; payload = Some mint_default_storage.payload ; amount_ = Some 100000000n} in
    let wallet = originate_wallet (turn_into_0value ) in
    (* minting *)
    let status = mint_  {wallet = wallet ; amount_ = 100tz ; mint = mint.addr}  Atom.start in
    let status = Atom.and 
      (Atom.assert_is_ok status  "sanity check : Minting should have succeeded")
      (Atom.assert_ ((Test.get_balance wallet.addr) = 0tz) "sanity check : wallet balance should be 0") in
    (* redeeming *)
    let status = redeem_  {wallet = wallet ; amount_ = 0tz ; mint = mint.addr} status in
    (* asserts *)
    Atom.and_list 
    [  Atom.assert_rejected_at status mint.addr "should have refused to redeem"  
    ;  Atom.assert_  ((Test.get_balance wallet.addr) = 0tz) "wallet balance should be 0" 
    ;  Atom.assert_  ((Test.get_balance mint.addr) = 100tz) "mint should have kept all funds" 
    ]
  end


(* we mint at 1 mint and redeem at another
  - should fail
  - no balance modification
*)
let _test_redeem_at_wrong_mint () = 
  begin
    log_ "test_redeem_at_wrong_mint";
    let mint_1 = originate_mint mint_default_storage in
    let mint_2 = originate_mint mint_default_storage in
    (* the wallet will check the ticket on reception*)
    let ticket_asserts : ticket_asserts = {no_assert with addr = Some mint_1.addr} in
    let wallet = originate_wallet (check_ticket ticket_asserts) in  
    (* minting *)
    let status = mint_  {wallet = wallet ; amount_ = 100tz ; mint = mint_1.addr}  Atom.start in
    let status = Atom.and 
      (Atom.assert_is_ok status "sanity check : Minting should have succeeded")
      (Atom.assert_  ((Test.get_balance wallet.addr) = 0tz) "sanity check : balance should be 0") in
    (* redeeming *)
    let status = redeem_  {wallet = wallet ; amount_ = 0tz ; mint = mint_2.addr}  status in
    (* asserts *)
    Atom.and_list 
    [  Atom.assert_rejected_at status mint_2.addr "should be rejected by second mint" 
    ;  Atom.assert_  ((Test.get_balance wallet.addr) = 0tz) "balance of wallet should be 0" 
    ;  Atom.assert_  ((Test.get_balance mint_1.addr) = 100tz) "balance of mint 1 should be 100tez" 
    ;  Atom.assert_  ((Test.get_balance mint_2.addr) = 0tz) "balance of mint 2 should be 0tez" 
    ]
  end 

(* Creation of test suite *)
let suite = Atom.make_suite
"Test suite of Mint SC"
[  Atom.make_test "Conversion tez / ticket" "conversion of mutez to number of ticket"  _test_inline_conversion_mutez           
;  Atom.make_test "Conversion tez / ticket" "conversion of arbitrary amount of tez to number of ticket" _test_inline_conversion_42tez              
;  Atom.make_test "Conversion ticket / tez" "conversion of 1 ticket into tez" _test_inline_conversion_chusai_1mutez      
;  Atom.make_test "Conversion ticket / tez" "conversion of necessary nb of ticket into 1 tez" _test_inline_conversion_chusai_tez         
;  Atom.make_test "Mint Origintation" "Test storage of Mint smart contract" _test_mint_origination                     
;  Atom.make_test "Mint of ticket" "Mint of a ticket, check of balances" _test_mint_first_ticket                    
;  Atom.make_test "Mint fails : less than min" "Fail to mint because amount sent is not enough" _test_mint_first_ticket_mutez 
;  Atom.make_test "Mint fails : 0 fund" "Fail to mint because no fund is sent" _test_mint_first_ticket_0tez               
;  Atom.make_test "Mint and redeem a ticket" "A ticket is mint, then redeemed at same mint" _test_mint_and_redeem                      
;  Atom.make_test "Redeem fail : 0 value" "Fails to redeem a 0-value ticket" _test_redeem_0value_ticket                 
;  Atom.make_test "Reddem fail : wrong mint" "Mint at mint 1, but try to redeem at other mint 2" _test_redeem_at_wrong_mint               
]

(* a test suite only used to illustrate what happens to the runner when a test fails*)
let suite2 = Atom.make_suite
"Test suite with artificial error to illustrate runner functionnality"
[  Atom.make_test "Conversion tez / ticket" "conversion of mutez to number of ticket"  _test_inline_conversion_mutez           
;  Atom.make_test "Conversion tez / ticket" "conversion of arbitrary amount of tez to number of ticket" _test_inline_conversion_42tez                  
;  Atom.make_test "Failing test" "This test artificially fails, just to illustrate how the runners works." (fun () -> Atom.fail "Error")                                
;  Atom.make_test "Redeem fail : 0 value" "Fails to redeem a 0-value ticket" _test_redeem_0value_ticket                 
;  Atom.make_test "Reddem fail : wrong mint" "Mint at mint 1, but try to redeem at other mint 2" _test_redeem_at_wrong_mint               
]

