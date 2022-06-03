#include "../../src/wallet_sc.mligo"
#include "fakes.mligo"
#include "tools.mligo"
#import "../../../stdlib_ext/src/atomic_test.mligo" "Atom"

type main_mint_test_props = {
  wallet_storage : wallet_storage;
  wallet_balance : tez;
  mint_balance : tez;
}

let run_main_mint_xtz_test 
    (body : wallet_parameter contract -> Atom.status) 
    (run_assertions : Atom.status -> main_mint_test_props -> Atom.status) = 
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
 
let _test_Wallet_sc_mint_xtz_with_0tez () = 
  run_main_mint_xtz_test 
    (fun (contr : wallet_parameter contract) -> Atom.transfer_to_contract_ contr Mint_xtz 0tez)
    (fun (result:Atom.status) ({wallet_storage; wallet_balance; mint_balance} : main_mint_test_props) -> 
      let {mint_address; bridge_address; ticket_storage} = wallet_storage in
      Atom.and_list 
      [ Atom.assert_rejected_with_error result (Test.compile_value "wallet_sc:Amount should be non-zero") "Assertion failed: should have been rejected"
      ; Atom.assert_equals ticket_storage (None : chusai_ticket_storage) "Should be empty"
      ; Atom.assert_equals wallet_balance 0tez "Should be empty"
      ]
    )

 
let _test_Wallet_sc_mint_xtz_with_10tez () =
  run_main_mint_xtz_test
    (fun (contr : wallet_parameter contract) -> Atom.transfer_to_contract_ contr Mint_xtz 10tez)
    (fun (result : Atom.status ) ({wallet_storage; wallet_balance; mint_balance} : main_mint_test_props) -> 
      Atom.and_list 
      [ Atom.assert_is_ok result "transaction should have succeeded"
      ; Atom.assert_equals (Wallet.extract_ticket_from_storage wallet_storage) 10n "there should be some tickets"
      ; Atom.assert_equals mint_balance 10tez "Should be not empty"
      ; Atom.assert_equals wallet_balance 0tez "Should be empty"
      ])

let _test_Wallet_sc_minted_ticket_and_join_with_existed_ticket_in_storage () =
  run_main_mint_xtz_test 
    (fun (contr : wallet_parameter contract) ->
      let status = Atom.transfer_to_contract_ contr Mint_xtz 15tez in
      Atom.transfer_to_contract status contr Mint_xtz 10tez)
    (fun (result : Atom.status) ({wallet_storage; wallet_balance; mint_balance} : main_mint_test_props) -> 
      Atom.and_list 
      [ Atom.assert_is_ok result "transaction should have succeeded"
      ; Atom.assert_equals (Wallet.extract_ticket_from_storage wallet_storage) 25n "there should be some tickets"
      ; Atom.assert_equals mint_balance 25tez "Should be not empty"
      ; Atom.assert_equals wallet_balance 0tez "Should be empty"
      ])

let _test_Wallet_sc_join_arbitary_ticket_and_ticket_in_storage () =
  run_main_mint_xtz_test 
    (fun (contr : wallet_parameter contract) ->
      let ticket = create_ticket (Bytes.pack "test") 10n in
      let status = Atom.transfer_to_contract_ contr Mint_xtz 10tez in
      Atom.transfer_to_contract status contr (Mint_xtz_cb ticket) 0tez)
    (fun (result : Atom.status) ({wallet_storage; wallet_balance; mint_balance} : main_mint_test_props) -> 
      let {mint_address; bridge_address; ticket_storage} = wallet_storage in
      Atom.and_list 
      [ Atom.assert_rejected_with_error result (Test.compile_value "wallet_sc:Ticket payload is invalid") "Assertion failed: should have been rejected"
      ; Atom.assert_equals (Wallet.extract_ticket_from_storage wallet_storage) 10n "there should be some tickets"
      ; Atom.assert_equals mint_balance 10tez "Should be not empty"
      ; Atom.assert_equals wallet_balance 0tez "Should be empty"
      ])


let suite = Atom.make_suite
"Wallet_sc: Test suite for Mint_xtz endpoint"
[ Atom.make_test "Mint fail : 0tez" "Should refuse to mint for 0tez"  _test_Wallet_sc_mint_xtz_with_0tez  
; Atom.make_test "Mint ok" "should store a ticket" _test_Wallet_sc_mint_xtz_with_10tez
; Atom.make_test "Mint ok" "should store sum of tickets" _test_Wallet_sc_minted_ticket_and_join_with_existed_ticket_in_storage
; Atom.make_test "Mint fail" "invalid payload for ticket" _test_Wallet_sc_join_arbitary_ticket_and_ticket_in_storage
]