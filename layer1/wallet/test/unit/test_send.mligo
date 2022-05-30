#include "../../../stdlib_ext/src/stdlibtestext.mligo"
#include "../../src/wallet_sc.mligo"
#include "fakes.mligo"
#include "tools.mligo"
#import "../../../stdlib_ext/src/atomic_test.mligo" "Atom"

(* Tests for wallet_sc/send *)

type main_send_test_props = {
  wallet_storage : wallet_storage;
  bridge_storage : bridge_storage;
}

let run_main_send_test
    (wallet_ticket : chusai_ticket option)
    (body : wallet_parameter contract -> chusai_ticket_storage -> Atom.status)  
    (run_assertions : Atom.status -> main_send_test_props -> Atom.status) = 

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
  let exec_result = body wallet_contract wallet_initial_storage.ticket_storage in

  let wallet_storage = Test.get_storage wallet_type_address in
  let bridge_storage = Test.get_storage bridge_type_address in

  run_assertions exec_result
    { wallet_storage = wallet_storage; 
      bridge_storage = bridge_storage;
    } 
 
let _test_Wallet_sc_sending () =
   let amount_to_deposit = 10n in
   let wallet_ticket = Some (create_ticket dummy_payload amount_to_deposit) in
   run_main_send_test 
      wallet_ticket
      (fun (contr: wallet_parameter contract) (ticket : chusai_ticket_storage) ->
        Atom.transfer_to_contract_ contr Send 0tez)
      (fun (result : Atom.status) ({wallet_storage;bridge_storage} : main_send_test_props) -> 
        Atom.and_list
        [ Atom.assert_is_ok result ""
        ; Atom.assert_equals (Wallet.extract_ticket_from_storage  wallet_storage) 0n ""
        ; Atom.assert_equals (Bridge.extract_ticket_from_storage  bridge_storage) amount_to_deposit ""
        ])
 
let _test_Wallet_sc_sending_when_storage_is_none () =
   run_main_send_test 
     (None : chusai_ticket option)
     (fun (contr: wallet_parameter contract) (ticket : chusai_ticket_storage) ->
        Atom.transfer_to_contract_ contr Send 0tez)
     (fun (result : Atom.status) ({wallet_storage;bridge_storage} : main_send_test_props) -> 
        Atom.and_list
        [ Atom.assert_rejected_with_error result (Test.compile_value "wallet_sc:No ticket found in storage") ""
        ; Atom.assert_equals (Wallet.extract_ticket_from_storage wallet_storage) 0n ""
        ; Atom.assert_equals (Bridge.extract_ticket_from_storage bridge_storage) 0n ""
        ])

let suite = Atom.make_suite
"Wallet_sc: Test suite for Send endpoint"
[ Atom.make_test "Send ok" "sends a simple ticket"  _test_Wallet_sc_sending  
; Atom.make_test "Send fail" "fails if no ticket" _test_Wallet_sc_sending_when_storage_is_none
]