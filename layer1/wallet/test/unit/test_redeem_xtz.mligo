#include "../../src/wallet_sc.mligo"
#include "fakes.mligo"
#include "tools.mligo"
#import "../../../stdlib_ext/src/unit_test.mligo" "Unit"

type main_redeem_test_props = {
  wallet_storage : wallet_storage;
  wallet_balance : tez;
  mint_balance : tez;
}

let run_main_redeem_xtz_test 
    (initial_mint_amount : tez)
    (ticket_opt : Ticket.t option)
    (body : wallet_parameter contract -> Unit.result) 
    (run_assertions : Unit.result -> main_redeem_test_props -> Unit.result) : Unit.result = 
  let mint_type_address, _, _ = Test.originate fake_mint_main unit initial_mint_amount in
  let mint_contract = Test.to_contract mint_type_address in
  let mint_address = (Tezos.address mint_contract) in


  let wallet_initial_storage = {
    owner_address = (Tezos.get_self_address ());
    mint_address = mint_address;
    bridge_address = dummy_address;
    ticket_storage = ticket_opt
  } in

  let wallet_type_address, _, _ = Test.originate main wallet_initial_storage 0tez in
  let wallet_contract = Test.to_contract wallet_type_address in

  let wallet_storage_before_body = Test.get_storage wallet_type_address in
  let exec_result = body wallet_contract in

  let wallet_storage = Test.get_storage wallet_type_address in
  let wallet_balance = Test.get_balance (Tezos.address wallet_contract) in
  let mint_balance = Test.get_balance mint_address in
  
  run_assertions  
    exec_result
    { wallet_storage = wallet_storage;
      wallet_balance = wallet_balance;
      mint_balance = mint_balance;
    } 

let _test_Wallet_sc_redeem_xtz_with_ticket () =
  let ticket_amount = 10n in
  let ticket = Ticket.create_ticket dummy_address dummy_payload ticket_amount in
  run_main_redeem_xtz_test 
    (ticket_amount * 1tez)
    (Some ticket)
    (fun (contr : wallet_parameter contract) ->Unit.transfer_to_contract_ contr Redeem_xtz 0tez)
    (fun (result:Unit.result) ({wallet_storage; wallet_balance; mint_balance} : main_redeem_test_props) -> 
      let {owner_address; mint_address; bridge_address; ticket_storage} = wallet_storage in
      Unit.and_list
      [ Unit.assert_is_ok result ""
      ; Unit.assert_equals ticket_storage (None : chusai_ticket_storage) "" 
      ; Unit.assert_equals wallet_balance  (ticket_amount * 1tez)  ""
      ; Unit.assert_equals mint_balance 0tez ""
      ])

let _test_Wallet_sc_redeem_xtz_with_storage_None () =
  let ticket_amount = 10n in
  run_main_redeem_xtz_test 
    (ticket_amount * 1tez)
    (None : Ticket.t option)
    (fun (contr: wallet_parameter contract) ->
      Unit.transfer_to_contract_ contr Redeem_xtz 0tez)
    (fun (result:Unit.result) ({wallet_storage; wallet_balance; mint_balance} : main_redeem_test_props) -> 
      Unit.and_list 
      [ Unit.assert_rejected_with_error result (Test.compile_value "wallet_sc:No ticket found in storage") "Assertion failed: should have been rejected"
      ; Unit.assert_equals wallet_balance  0tez  ""
      ; Unit.assert_equals mint_balance (ticket_amount * 1tez)  ""
      ])


let suite = Unit.make_suite
"Wallet_sc"
"Test suite for Redeem_xtz endpoint"
[ Unit.make_test "Redeem ok" "redeem a simple ticket"  _test_Wallet_sc_redeem_xtz_with_ticket  
; Unit.make_test "Redeem fail" "Redeem_xtz fails if no ticket" _test_Wallet_sc_redeem_xtz_with_storage_None
]