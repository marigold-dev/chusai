#include "../../src/wallet_sc.mligo"
#include "fakes.mligo"
#include "tools.mligo"
#include "../../../stdlib_ext/src/stdlibtestext.mligo"

type main_mint_test_props = {
  wallet_storage : wallet_storage;
  wallet_balance : tez;
  mint_balance : tez;
}

let run_main_mint_xtz_test 
    (body : wallet_parameter contract -> test_exec_result list) 
    (run_assertions : test_exec_result list -> main_mint_test_props -> unit) = 
  let mint_type_address, _, _ = Test.originate fake_mint_main unit 0tez in
  let mint_contract = Test.to_contract mint_type_address in
  let mint_address = (Tezos.address mint_contract) in


  let wallet_initial_storage = {
    mint_address = mint_address;
    bridge_address = dummy_address;
    ticket_storage = (None : chusai_ticket_storage)
  } in

  let wallet_type_address, _, _ = Test.originate main wallet_initial_storage 0tez in
  let wallet_contract = Test.to_contract wallet_type_address in

  let wallet_storage_before_body = Test.get_storage wallet_type_address in
  let exec_results = body wallet_contract in

  let wallet_storage = Test.get_storage wallet_type_address in
  let wallet_balance = Test.get_balance (Tezos.address wallet_contract) in
  let mint_balance = Test.get_balance mint_address in
  let _ = run_assertions 
    exec_results
    { wallet_storage = wallet_storage; 
      wallet_balance = wallet_balance;
      mint_balance = mint_balance;
    } in
  unit

let test_Wallet_sc_mint_xtz_with_0tez =
  run_main_mint_xtz_test 
    (fun (contr : wallet_parameter contract) -> [Test.transfer_to_contract contr Mint_xtz 0tez])
    (fun (exec_results:test_exec_result list) ({wallet_storage; wallet_balance; mint_balance} : main_mint_test_props) -> 
      let _ = 
        TestExt.assert_cond 
          exec_results 
          (fun (results : test_exec_result list) -> 
            let expected_error : michelson_program = Test.compile_value "wallet_sc:Amount should be non-zero" in 
            match results with 
              [Fail (Rejected (error , _))] -> if error = expected_error then true else false
              | _ -> false) in
      let {mint_address; bridge_address; ticket_storage} = wallet_storage in
      let _ = TestExt.assert_equals ticket_storage (None : chusai_ticket_storage) in
      let _ = TestExt.assert_equals wallet_balance  0tez in
      unit)


let test_Wallet_sc_mint_xtz_with_10tez =
  run_main_mint_xtz_test
    (fun (contr : wallet_parameter contract) -> [Test.transfer_to_contract contr Mint_xtz 10tez])
    (fun (_:test_exec_result list) ({wallet_storage; wallet_balance; mint_balance} : main_mint_test_props) -> 
      let {mint_address; bridge_address; ticket_storage} = wallet_storage in
      let _ = TestExt.assert_equals (Wallet.extract_ticket_from_storage wallet_storage) 10n in
      let _ = TestExt.assert_equals mint_balance 10tez in
      let _ = TestExt.assert_equals wallet_balance 0tez in 
      unit)

let test_Wallet_sc_minted_ticket_and_join_with_existed_ticket_in_storage =
  run_main_mint_xtz_test 
    (fun (contr : wallet_parameter contract) ->
      [ Test.transfer_to_contract contr Mint_xtz 15tez;
        Test.transfer_to_contract contr Mint_xtz 10tez
      ])
    (fun (_:test_exec_result list) ({wallet_storage; wallet_balance; mint_balance} : main_mint_test_props) -> 
      let {mint_address; bridge_address; ticket_storage} = wallet_storage in
      let _ = TestExt.assert_equals (Wallet.extract_ticket_from_storage  wallet_storage) 25n in
      let _ = TestExt.assert_equals mint_balance 25tez in
      let _ = TestExt.assert_equals wallet_balance 0tez in
      unit)

let test_Wallet_sc_join_arbitary_ticket_and_ticket_in_storage =
  run_main_mint_xtz_test 
    (fun (contr : wallet_parameter contract) ->
      let ticket = create_ticket (Bytes.pack "test") 10n in
      [ Test.transfer_to_contract contr Mint_xtz 10tez;
        Test.transfer_to_contract contr (Mint_xtz_cb ticket) 0tez])
    (fun (exec_results: test_exec_result list) ({wallet_storage; wallet_balance; mint_balance} : main_mint_test_props) -> 
      let {mint_address; bridge_address; ticket_storage} = wallet_storage in
      let _ = 
        TestExt.assert_cond 
          exec_results 
          (fun (results : test_exec_result list) -> 
            let expected_error : michelson_program = Test.compile_value "wallet_sc:Ticket payload is invalid" in 
            match results with 
              [Success _ ; Fail (Rejected (error , _))] -> if error = expected_error then true else false
              | _ -> false ) in
      let _ = TestExt.assert_equals (Wallet.extract_ticket_from_storage  wallet_storage) 10n in
      let _ = TestExt.assert_equals mint_balance 10tez in
      let _ = TestExt.assert_equals wallet_balance 0tez in
      unit)