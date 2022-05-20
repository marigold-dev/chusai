#include "../../../stdlib_ext/src/stdlibtestext.mligo"
#include "../../src/wallet_sc.mligo"
#include "fakes.mligo"
#include "tools.mligo"

type main_redeem_test_props = {
  wallet_storage : wallet_storage;
  wallet_balance : tez;
  mint_balance : tez;
}

let run_main_redeem_xtz_test 
    (initial_mint_amount : tez)
    (ticket_opt : chusai_ticket option)
    (body : wallet_parameter contract -> test_exec_result list) 
    (run_assertions : test_exec_result list -> main_redeem_test_props -> unit) : unit = 
  let mint_type_address, _, _ = Test.originate fake_mint_main unit initial_mint_amount in
  let mint_contract = Test.to_contract mint_type_address in
  let mint_address = (Tezos.address mint_contract) in


  let wallet_initial_storage = {
    mint_address = mint_address;
    bridge_address = dummy_address;
    ticket_storage = ticket_opt
  } in

  let wallet_type_address, _, _ = Test.originate main wallet_initial_storage 0tez in
  let wallet_contract = Test.to_contract wallet_type_address in

  let wallet_storage_before_body = Test.get_storage wallet_type_address in
  let exec_results = body wallet_contract in

  let wallet_storage = Test.get_storage wallet_type_address in
  let wallet_balance = Test.get_balance (Tezos.address wallet_contract) in
  let mint_balance = Test.get_balance mint_address in
  
  run_assertions 
    exec_results
    { wallet_storage = wallet_storage;
      wallet_balance = wallet_balance;
      mint_balance = mint_balance;
    } 

let test_Wallet_sc_redeem_xtz_with_ticket =
  let ticket_amount = 10n in
  let ticket = create_ticket dummy_payload ticket_amount in
  run_main_redeem_xtz_test 
    (ticket_amount * 1tez)
    (Some ticket)
    (fun (contr : wallet_parameter contract) ->[Test.transfer_to_contract contr Redeem_xtz 0tez])
    (fun (_:test_exec_result list) ({wallet_storage; wallet_balance; mint_balance} : main_redeem_test_props) -> 
      let {mint_address; bridge_address; ticket_storage} = wallet_storage in
      let _ = TestExt.assert_equals ticket_storage (None : chusai_ticket_storage) in
      let _ = TestExt.assert_equals wallet_balance  (ticket_amount * 1tez) in
      let _ = TestExt.assert_equals mint_balance  0tez in
      unit)

let test_Wallet_sc_redeem_xtz_with_storage_None =
  let ticket_amount = 10n in
  run_main_redeem_xtz_test 
    (ticket_amount * 1tez)
    (None : chusai_ticket option)
    (fun (contr: wallet_parameter contract) ->
      [Test.transfer_to_contract contr Redeem_xtz 0tez])
    (fun (exec_results:test_exec_result list) ({wallet_storage; wallet_balance; mint_balance} : main_redeem_test_props) -> 
      let _ =
        TestExt.assert_cond 
          exec_results 
          (fun (results : test_exec_result list) -> 
            let expected_error : michelson_program = Test.compile_value "wallet_sc:No ticket found in storage" in 
            match results with 
              [Fail (Rejected (error , _))] -> if error = expected_error then true else false
              | _ -> false ) in
      let _ = TestExt.assert_equals wallet_balance  0tez in
      let _ = TestExt.assert_equals mint_balance (ticket_amount * 1tez) in
      unit)
