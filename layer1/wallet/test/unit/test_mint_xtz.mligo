#include "../../src/wallet_sc.mligo"
#include "fakes.mligo"
#include "tools.mligo"
#import "../../../stdlib_ext/src/unit_test.mligo" "Unit"
#import "../../../stdlib_ext/src/proxy_ticket.mligo" "Proxy_ticket"
#import "../../../stdlib_ext/src/result.mligo" "Result"

type main_mint_test_props = {
  wallet_storage : wallet_storage_human;
  wallet_balance : tez;
  mint_balance : tez;
}

let extract_ticket_from_storage (x : wallet_storage_human) : nat =
    OptionExt.default
      (Option.map
        (fun (ticket : Ticket.payload human_ticket) ->
          let {amount ; value = _ ; ticketer = _ } = ticket in
          amount
        )
        x.ticket_storage)
      0n

let dummy_ticket_info = (0x00,0n)

let run_main_mint_xtz_test 
    (body : wallet_parameter contract -> Unit.result) 
    (run_assertions : Unit.result -> main_mint_test_props -> Unit.result) = 

  let mint_type_address, _, _ = Test.originate fake_mint_main unit 0tez in
  let mint_contract = Test.to_contract mint_type_address in
  let mint_address = (Tezos.address mint_contract) in

  let owner_address = let x = "WARNING: Test.get_self_address is weird here. What I write might be better but not sure" in Test.nth_bootstrap_account 1 in
  let wallet_initial_storage ( _ : bytes ticket) = {
    // owner_address = (Tezos.get_self_address ());
    owner_address = owner_address ;
    mint_address = mint_address;
    bridge_address = dummy_address;
    ticket_storage = (None : bytes ticket option)
  } in

  let wallet_address : address = Proxy_ticket.originate dummy_ticket_info wallet_initial_storage main in
  let wallet_type_address = (Test.cast_address wallet_address : (wallet_parameter, wallet_storage) typed_address) in
  let wallet_contract = Test.to_contract wallet_type_address in
  // let wallet_storage_before_body = Test.get_storage wallet_type_address in
  let exec_result = body wallet_contract in

  let wallet_storage = (Test.decompile (Test.get_storage_of_address wallet_address) : wallet_storage_human) in
  let wallet_balance = Test.get_balance (Tezos.address wallet_contract) in
  let mint_balance = Test.get_balance mint_address in
  run_assertions 
    exec_result
    { wallet_storage = wallet_storage; (* ticket ? *)
      wallet_balance = wallet_balance;
      mint_balance = mint_balance;
    }
 
let _test_Wallet_sc_mint_xtz_with_0tez () = 
  run_main_mint_xtz_test 
    (fun (contr : wallet_parameter contract) -> Unit.transfer_to_contract_ contr Mint_xtz 0tez)
    (fun (result:Unit.result) ({wallet_storage; wallet_balance; mint_balance = _} : main_mint_test_props) -> 
      let {owner_address = _ ; mint_address = _; bridge_address = _; ticket_storage} = wallet_storage in
      Unit.and_list 
      [ Unit.assert_rejected_with_error result (Test.compile_value "wallet_sc:Amount should be non-zero") "Assertion failed: should have been rejected"
      ; Unit.assert_equals ticket_storage (None : Ticket.payload human_ticket option) "Should be empty"
      ; Unit.assert_equals wallet_balance 0tez "Should be empty"
      ]
    )

 
let _test_Wallet_sc_mint_xtz_with_10tez () =
  run_main_mint_xtz_test
    (fun (contr : wallet_parameter contract) -> Unit.transfer_to_contract_ contr Mint_xtz 10tez)
    (fun (result : Unit.result ) ({wallet_storage; wallet_balance; mint_balance} : main_mint_test_props) -> 
      Unit.and_list 
      [ Unit.assert_is_ok result "transaction should have succeeded"
      ; Unit.assert_equals (extract_ticket_from_storage wallet_storage) 10n "there should be some tickets"
      ; Unit.assert_equals mint_balance 10tez "Should be not empty"
      ; Unit.assert_equals wallet_balance 0tez "Should be empty"
      ])

let _test_Wallet_sc_minted_ticket_and_join_with_existed_ticket_in_storage () =
  run_main_mint_xtz_test 
    (fun (contr : wallet_parameter contract) ->
      let status = Unit.transfer_to_contract_ contr Mint_xtz 15tez in
      Unit.transfer_to_contract status contr Mint_xtz 10tez)
    (fun (result : Unit.result) ({wallet_storage; wallet_balance; mint_balance} : main_mint_test_props) -> 
      Unit.and_list 
      [ Unit.assert_is_ok result "transaction should have succeeded"
      ; Unit.assert_equals (extract_ticket_from_storage wallet_storage) 25n "there should be some tickets"
      ; Unit.assert_equals mint_balance 25tez "Should be not empty"
      ; Unit.assert_equals wallet_balance 0tez "Should be empty"
      ])

let _test_Wallet_sc_join_arbitary_ticket_and_ticket_in_storage () =
  run_main_mint_xtz_test 
    (fun (contr : wallet_parameter contract) ->
      let status1 = Unit.transfer_to_contract_ contr Mint_xtz 10tez in
      let tx_ticket () =
        let addr_proxy = Proxy_ticket.init_transfer (fun (t:bytes ticket) -> Mint_xtz_cb t) in
        let ticket_data = (Bytes.pack "test" , 10n) in
        let addr = Tezos.address contr in
        Proxy_ticket.transfer addr_proxy (ticket_data,addr)
      in
      let operation () = Unit.try_with tx_ticket in
      Unit.and_lazy status1 operation
    )
    (fun (result : Unit.result) ({wallet_storage; wallet_balance; mint_balance} : main_mint_test_props) -> 
      let {owner_address = _ ; mint_address = _ ; bridge_address = _; ticket_storage = _} = wallet_storage in
      Unit.and_list 
      [ Unit.assert_rejected_with_error result (Test.compile_value "wallet_sc:Ticket payload is invalid") "Assertion failed: should have been rejected"
      ; Unit.assert_equals (extract_ticket_from_storage wallet_storage) 10n "there should be some tickets"
      ; Unit.assert_equals mint_balance 10tez "Should be not empty"
      ; Unit.assert_equals wallet_balance 0tez "Should be empty"
      ])


let suite = Unit.make_suite
"Wallet_sc"
"Test suite for Mint_xtz endpoint"
[ Unit.make_test "Mint fail : 0tez" "Should refuse to mint for 0tez"  _test_Wallet_sc_mint_xtz_with_0tez  
; Unit.make_test "Mint ok" "should store a ticket" _test_Wallet_sc_mint_xtz_with_10tez
; Unit.make_test "Mint ok" "should store sum of tickets" _test_Wallet_sc_minted_ticket_and_join_with_existed_ticket_in_storage
; Unit.make_test "Mint fail" "invalid payload for ticket" _test_Wallet_sc_join_arbitary_ticket_and_ticket_in_storage
]