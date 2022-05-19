#include "../../src/wallet_sc.mligo"
#include "fakes.mligo"
#include "tools.mligo"

(* Tests for wallet_sc/send *)

type main_send_test_props = {
  wallet_storage : wallet_storage;
  bridge_storage : bridge_storage;
}

let run_main_send_test
    (wallet_ticket : chusai_ticket option)
    (body : wallet_parameter contract -> chusai_ticket_storage -> test_exec_result list)  
    (run_assertions : test_exec_result list -> main_send_test_props -> unit) = 

  let bridge_initial_storage = {
    tickets = (None : chusai_ticket option);
  } in

  let bridge_type_address, _, _ = Test.originate fake_bridge_main bridge_initial_storage 0tez in
  let bridge_contract = Test.to_contract bridge_type_address in
  let bridge_address = (Tezos.address bridge_contract) in
  
  let wallet_initial_storage = {
    mint_address = dummy_address;
    bridge_address = bridge_address;
    ticket_storage = wallet_ticket
  } in

  let wallet_type_address, _, _ = Test.originate main wallet_initial_storage 0tez in
  let wallet_contract = Test.to_contract wallet_type_address in

  let wallet_storage_before_body = Test.get_storage wallet_type_address in
  let exec_results = body wallet_contract wallet_initial_storage.ticket_storage in

  let wallet_storage = Test.get_storage wallet_type_address in
  let bridge_storage = Test.get_storage bridge_type_address in

  let _ = run_assertions exec_results
    { wallet_storage = wallet_storage; 
      bridge_storage = bridge_storage;
    } in
  unit
 
let test_Wallet_sc_sending =
   let amount_to_deposit = 10n in
   let wallet_ticket = Some (create_ticket dummy_payload amount_to_deposit) in
   run_main_send_test 
      wallet_ticket
      (fun (contr: wallet_parameter contract) (ticket : chusai_ticket_storage) ->
        [Test.transfer_to_contract contr Send 0tez])
      (fun (exec_result:test_exec_result list) ({wallet_storage;bridge_storage} : main_send_test_props) -> 
        let _ = assert (Wallet.extract_ticket_from_storage  wallet_storage = 0n) in
        assert (Bridge.extract_ticket_from_storage  bridge_storage  = amount_to_deposit))
 
let test_Wallet_sc_sending_when_storage_is_none =
   run_main_send_test 
     (None : chusai_ticket option)
     (fun (contr: wallet_parameter contract) (ticket : chusai_ticket_storage) ->
        [Test.transfer_to_contract contr Send 0tez])
     (fun (exec_result:test_exec_result list) ({wallet_storage;bridge_storage} : main_send_test_props) -> 
        let _ = assert (match exec_result with [Fail (Rejected _)] -> true | v -> false) in
        let _ = assert (Wallet.extract_ticket_from_storage  wallet_storage = 0n) in
       assert (Bridge.extract_ticket_from_storage bridge_storage  = 0n))
