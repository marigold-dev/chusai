#include "../src/mint_sc.mligo"
#include "test_utils.mligo"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
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
type wallet_storage = Ticket.t option
type wallet_parameter =
      Store of Ticket.t // callback to receive ticket
    | Go_mint of address   // to start minting process from implicit account
    | Go_redeem of address // to start redeem process from implicit account
    | Nope                 // callback to receive money
let wallet_test_main 
  (check_ticket : Ticket.t-> Ticket.t) 
  (action, store : wallet_parameter * wallet_storage) 
    : operation list * wallet_storage = 
    match action with
    | Store ticket ->         
        let (addr, (payload, total)), ticket = Ticket.read_ticket ticket in
        begin
          let ticket = check_ticket ticket in
          [], (Some ticket)
        end
    | Nope -> 
      [], store
    | Go_mint addr -> 
        let mint_contr : mint_parameter contract= Tezos.get_contract_with_error addr "No mint to mint" in
        let mint_cb : Ticket.t contract = Tezos.self "%store" in
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
// FIXME: it is necessary to redefine originated separately (Unit.orignated does work in the definition of originated wallet (for now, ligo 0.43.0))
type originated = Unit.originated
type originated_wallet = (wallet_parameter, wallet_storage) originated
(* originates a wallet, given a function to be applied on the ticket when receiving one *)
let originate_wallet (injected_logic : Ticket.t-> Ticket.t) : originated_wallet = 
    Unit.originate 
       (wallet_test_main injected_logic)
       (None : wallet_storage)
       0tez
      

type originated_mint = (mint_parameter, storage) originated
let mint_default_storage = {payload = 0x00 ; minimum_amount = 1tez}
(* originates a mint *)
let originate_mint (store : storage) : originated_mint = Unit.originate main store 0tez



(* *********************************** *)
(* Simplified calls to Mint and Redeem *)
type mint_param = 
{
  wallet : originated_wallet;
  amount_ : tez ; //0tz when redeeming (not used)
  mint : address 
}

let mint_ (param : mint_param) (previous : Unit.result)=
  Unit.transfer_to_contract previous param.wallet.originated_contract (Go_mint param.mint) param.amount_ 

let redeem_ (param : mint_param) (previous : Unit.result)=
  Unit.transfer_to_contract previous param.wallet.originated_contract (Go_redeem param.mint) 0tz  


(* *********************************** *)
(* TESTS *)

(* Tests of pure functions *)
let _test_inline_conversion_mutez  () =  Unit.assert_ ((xtz_to_chusai_amount 1mutez) = 1n) "1 ticket per mutez"

let _test_inline_conversion_42tez  () =  Unit.assert_ ((xtz_to_chusai_amount 42tez) = 42000000n) "1 ticket per mutez"

let _test_inline_conversion_chusai_tez  () =  Unit.assert_ ((chusai_amount_to_xtz 1000000n) = 1tez) "1 mutez per ticket" 

let _test_inline_conversion_chusai_1mutez  () =  Unit.assert_ ((chusai_amount_to_xtz 1n) = 1mutez) "1 mutez per ticket"

(* simple check of initial storage/balance *)
let _test_mint_origination  () = 
  begin
    let mint = originate_mint mint_default_storage in
    Unit.assert_ ((Test.get_balance mint.originated_address) = 0tz) "balance should be 0"  
  end

(* we sent an amount to mint
   - we got a ticket with correct mint, payload and amount
   - balance of mint is ok
*)
let _test_mint_first_ticket  () = 
  begin
    let mint = originate_mint mint_default_storage in
    (* we check in wallet the tickets upon reception *)
    let ticket_asserts : ticket_asserts = {addr = Some mint.originated_address ; payload = Some mint_default_storage.payload ; amount_ = Some 1000000n } in
    let wallet = originate_wallet (check_ticket ticket_asserts) in
    (* minting *)
    let status = mint_  {wallet = wallet; amount_ = 1tz ; mint = mint.originated_address}  (Unit.start ()) in
    (* asserts *)
    Unit.and_list 
    [  Unit.assert_is_ok status "Mint transaction should be OK" 
    ;  Unit.assert_ ((Test.get_balance wallet.originated_address) = 0tz) "wallet balance should be 0" 
    ;  Unit.assert_ ((Test.get_balance mint.originated_address) = 1tz) "mint balance should be 1tez" 
    ] 
  end

(* we sent an inappropriate amount (less than minimum)
    - no ticket received
    - mint is not credited 
*)
let _test_mint_first_ticket_mutez () = 
  begin
    let mint = originate_mint {payload = 0x00 ; minimum_amount = 100tez} in // fixes minimum amount at 100tea
    let wallet = originate_wallet (fun (t : Ticket.t) -> t) in
    (* minting *)
    let status = mint_  {wallet = wallet ; amount_ = 1tez ; mint = mint.originated_address}  (Unit.start ()) in // 1tez is less than minimum (100tez)
    (* asserts *)
    Unit.and_list 
    [  Unit.assert_rejected_at status mint.originated_address "should be rejected 'cause less than minimum amount"  
    ;  Unit.assert_ (Stdlib.OptionExt.is_none (Test.get_storage wallet.originated_typed_address)) "there should be no ticket received"  
    ;  Unit.assert_ ((Test.get_balance mint.originated_address) = 0tz) "balance should be 0 cause no ticket provided"   
    ]
  end

(* we sent an inappropriate amount (0)
    - no ticket received
    - mint is not credited 
*)
let _test_mint_first_ticket_0tez () = 
  begin
    let mint = originate_mint mint_default_storage in
    let wallet = originate_wallet (fun (t : Ticket.t) -> t) in
    (* minting *)
    let status = mint_  {wallet = wallet ; amount_ = 0mutez ; mint = mint.originated_address}  (Unit.start ()) in
    (* asserts *)
    Unit.and_list 
    [  Unit.assert_rejected_at status mint.originated_address "should be rejected 'cause less than minimum amount" 
    ;  Unit.assert_ (Stdlib.OptionExt.is_none (Test.get_storage wallet.originated_typed_address )) "there should be no ticket received" 
    ;  Unit.assert_ ((Test.get_balance mint.originated_address) = 0tz) "balance should be 0 cause no ticket provided"   
    ]
  end

(* we mint and redeem at same Mint
  - balances should change
  - taxes should be deduced
  - ticket should be burned after redeeming
*)
let _test_mint_and_redeem () = 
  begin
    let mint = originate_mint mint_default_storage in
    (* the wallet will check the ticket on reception*)
    let ticket_asserts : ticket_asserts= {addr = Some mint.originated_address ; payload = Some mint_default_storage.payload ; amount_ = Some 100000000n} in
    let wallet = originate_wallet (check_ticket ticket_asserts) in
    (* minting *)
    let status = mint_ {wallet = wallet ; amount_ = 100tz ; mint = mint.originated_address}  (Unit.start ()) in
    let status = 
      Unit.and 
        (Unit.assert_is_ok status "sanity check : Minting should have succeeded") 
        (Unit.assert_  ((Test.get_balance wallet.originated_address) = 0tz) "sanity check : wallet balance should be 0")  in 
    (* redeeming *)
    let status = redeem_  {wallet = wallet ; amount_ = 0tz ; mint = mint.originated_address}  status in
    (* asserts *)
    Unit.and_list 
    [  Unit.assert_is_ok status "Redeem transaction should be OK" 
    ;  Unit.assert_  (Stdlib.OptionExt.is_none (Test.get_storage wallet.originated_typed_address)) "there should be no ticket left in wallet storage" 
    ;  Unit.assert_  ((Test.get_balance mint.originated_address) = 0tz) "mint should have nothing left" 
    ;  Unit.assert_  ((Test.get_balance wallet.originated_address) = 100tz) "wallet should have gotten xtz back"
    ]
  end

(* we try to redeem a 0-value ticket
  - mint should refuse to redeem
*)
(* function used to split ticket in wallet after minting, will produce a 0-value ticket *)
let turn_into_0value (ticket : Ticket.t) : Ticket.t = 
  let (_, (_, amount_)), ticket = Ticket.read_ticket ticket in
  let opt : (Ticket.t*Ticket.t) option = Ticket.split_ticket ticket 0n amount_  in
  let ticket0, _ticket = Option.unopt(opt) in
  ticket0
(* test *)
let _test_redeem_0value_ticket () = 
  begin
    let mint = originate_mint mint_default_storage in
    (* the wallet will check the ticket on reception*)
    let ticket_asserts : ticket_asserts= {addr = Some mint.originated_address ; payload = Some mint_default_storage.payload ; amount_ = Some 100000000n} in
    let wallet = originate_wallet (turn_into_0value ) in
    (* minting *)
    let status = mint_  {wallet = wallet ; amount_ = 100tz ; mint = mint.originated_address}  (Unit.start ()) in
    let status = Unit.and 
      (Unit.assert_is_ok status  "sanity check : Minting should have succeeded")
      (Unit.assert_ ((Test.get_balance wallet.originated_address) = 0tz) "sanity check : wallet balance should be 0") in
    (* redeeming *)
    let status = redeem_  {wallet = wallet ; amount_ = 0tz ; mint = mint.originated_address} status in
    (* asserts *)
    Unit.and_list 
    [  Unit.assert_rejected_at status mint.originated_address "should have refused to redeem"  
    ;  Unit.assert_  ((Test.get_balance wallet.originated_address) = 0tz) "wallet balance should be 0" 
    ;  Unit.assert_  ((Test.get_balance mint.originated_address) = 100tz) "mint should have kept all funds" 
    ]
  end


(* we mint at 1 mint and redeem at another
  - should fail
  - no balance modification
*)
let _test_redeem_at_wrong_mint () = 
  begin
    let mint_1 = originate_mint mint_default_storage in
    let mint_2 = originate_mint mint_default_storage in
    (* the wallet will check the ticket on reception*)
    let ticket_asserts : ticket_asserts = {no_assert with addr = Some mint_1.originated_address} in
    let wallet = originate_wallet (check_ticket ticket_asserts) in  
    (* minting *)
    let status = mint_  {wallet = wallet ; amount_ = 100tz ; mint = mint_1.originated_address}  (Unit.start ()) in
    let status = Unit.and 
      (Unit.assert_is_ok status "sanity check : Minting should have succeeded")
      (Unit.assert_  ((Test.get_balance wallet.originated_address) = 0tz) "sanity check : balance should be 0") in
    (* redeeming *)
    let status = redeem_  {wallet = wallet ; amount_ = 0tz ; mint = mint_2.originated_address}  status in
    (* asserts *)
    Unit.and_list 
    [  Unit.assert_rejected_at status mint_2.originated_address "should be rejected by second mint" 
    ;  Unit.assert_  ((Test.get_balance wallet.originated_address) = 0tz) "balance of wallet should be 0" 
    ;  Unit.assert_  ((Test.get_balance mint_1.originated_address) = 100tz) "balance of mint 1 should be 100tez" 
    ;  Unit.assert_  ((Test.get_balance mint_2.originated_address) = 0tz) "balance of mint 2 should be 0tez" 
    ]
  end 

(* Creation of test suite *)
let suite = Unit.make_suite
"Mint_sc"
"Test suite of Mint SC"
[  Unit.make_test "Conversion tez / ticket" "conversion of mutez to number of ticket"  _test_inline_conversion_mutez           
;  Unit.make_test "Conversion tez / ticket" "conversion of arbitrary amount of tez to number of ticket" _test_inline_conversion_42tez              
;  Unit.make_test "Conversion ticket / tez" "conversion of 1 ticket into tez" _test_inline_conversion_chusai_1mutez      
;  Unit.make_test "Conversion ticket / tez" "conversion of necessary nb of ticket into 1 tez" _test_inline_conversion_chusai_tez         
;  Unit.make_test "Mint Origintation" "Test storage of Mint smart contract" _test_mint_origination                     
;  Unit.make_test "Mint of ticket" "Mint of a ticket, check of balances" _test_mint_first_ticket                    
;  Unit.make_test "Mint fails : less than min" "Fail to mint because amount sent is not enough" _test_mint_first_ticket_mutez 
;  Unit.make_test "Mint fails : 0 fund" "Fail to mint because no fund is sent" _test_mint_first_ticket_0tez               
;  Unit.make_test "Mint and redeem a ticket" "A ticket is mint, then redeemed at same mint" _test_mint_and_redeem                      
;  Unit.make_test "Redeem fail : 0 value" "Fails to redeem a 0-value ticket" _test_redeem_0value_ticket                 
;  Unit.make_test "Reddem fail : wrong mint" "Mint at mint 1, but try to redeem at other mint 2" _test_redeem_at_wrong_mint               
]


