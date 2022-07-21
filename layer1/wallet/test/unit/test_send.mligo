#include "../../src/wallet_sc.mligo"
#include "fakes.mligo"
#include "tools.mligo"
#import "../../../stdlib_ext/src/unit_test.mligo" "Unit"
#import "../../../stdlib_ext/src/proxy_ticket.mligo" "Proxy_ticket"
let dummy_ticket_info = (0x00, 0n)

(* Tests for wallet_sc/send *)

type main_send_test_props = {
  wallet_storage : wallet_storage_human ;
  bridge_storage : bridge_storage_human ;
}

let get_storage_wallet (addr: (wallet_parameter, wallet_storage) typed_address) : wallet_storage_human =
  let x = Test.to_contract addr in
  let a : address = [%external ("ADDRESS", x)] in
  let x = Test.get_storage_of_address a in
  (Test.decompile x : wallet_storage_human)

let get_storage_bridge (addr: (bridge_parameter, bridge_storage) typed_address) : bridge_storage_human =
  let x = Test.to_contract addr in
  let a : address = [%external ("ADDRESS", x)] in
  let x = Test.get_storage_of_address a in
  (Test.decompile x : bridge_storage_human)

let run_main_send_test
    (wallet_ticket : (Ticket.payload * nat) option)
    (body : wallet_parameter contract -> Ticket.payload human_ticket option -> Unit.result)  
    (run_assertions : Unit.result -> main_send_test_props -> Unit.result) = 


  let bridge_address =
    let bridge_initial_storage (_ : Ticket.t) = { tickets = (None : Ticket.t option) } in
    Proxy_ticket.originate dummy_ticket_info bridge_initial_storage fake_bridge_main
  in
  let bridge_type_address = (Test.cast_address bridge_address : (bridge_parameter,bridge_storage) typed_address) in


  let wallet_address =
    match wallet_ticket with
    | None ->
      let wallet_initial_storage (_ : Ticket.t) = {
        owner_address = (Tezos.get_self_address ());
        mint_address = dummy_address;
        bridge_address = bridge_address;
        ticket_storage = (None : Ticket.t option)
      } in
      Proxy_ticket.originate dummy_ticket_info wallet_initial_storage main
    | Some ticket_info ->
      let wallet_initial_storage (t : Ticket.t) = {
        owner_address = (Tezos.get_self_address ());
        mint_address = dummy_address;
        bridge_address = bridge_address;
        ticket_storage = Some t
      } in
      Proxy_ticket.originate ticket_info wallet_initial_storage main
  in
  let wallet_type_address = (Test.cast_address wallet_address : (wallet_parameter,wallet_storage) typed_address) in
  let wallet_contract = Test.to_contract wallet_type_address in

  let wallet_storage_before_body = get_storage_wallet wallet_type_address in
  let exec_result = body wallet_contract wallet_storage_before_body.ticket_storage in

  let wallet_storage = get_storage_wallet wallet_type_address in
  let bridge_storage = get_storage_bridge bridge_type_address in

  run_assertions exec_result
    { wallet_storage = wallet_storage; 
      bridge_storage = bridge_storage;
    } 
 
let _test_Wallet_sc_sending () =
   let amount_to_deposit = 10n in
   let wallet_ticket = Some (dummy_payload, amount_to_deposit) in
   run_main_send_test 
      wallet_ticket
      (fun (contr: wallet_parameter contract) (_ticket : Ticket.payload human_ticket option) ->
        Unit.transfer_to_contract_ contr Send 0tez)
      (fun (result : Unit.result) ({wallet_storage;bridge_storage} : main_send_test_props) -> 
        Unit.and_list
        [ Unit.assert_is_ok result ""
        ; Unit.assert_equals (Wallet.extract_ticket_from_storage  wallet_storage) 0n ""
        ; Unit.assert_equals (Bridge.extract_ticket_from_storage  bridge_storage) amount_to_deposit ""
        ])
 
let _test_Wallet_sc_sending_when_storage_is_none () =
   run_main_send_test 
     (None : (Ticket.payload * nat) option)
     (fun (contr: wallet_parameter contract) (_ticket : Ticket.payload human_ticket option) ->
        Unit.transfer_to_contract_ contr Send 0tez)
     (fun (result : Unit.result) ({wallet_storage;bridge_storage} : main_send_test_props) -> 
        Unit.and_list
        [ Unit.assert_rejected_with_error result (Test.compile_value "wallet_sc:No ticket found in storage") ""
        ; Unit.assert_equals (Wallet.extract_ticket_from_storage wallet_storage) 0n ""
        ; Unit.assert_equals (Bridge.extract_ticket_from_storage bridge_storage) 0n ""
        ])

let suite = Unit.make_suite
"Wallet_sc" 
"Test suite for Send endpoint"
[ Unit.make_test "Send ok" "sends a simple ticket"  _test_Wallet_sc_sending  
; Unit.make_test "Send fail" "fails if no ticket" _test_Wallet_sc_sending_when_storage_is_none
]

let test1 = _test_Wallet_sc_sending ()
let test2 = _test_Wallet_sc_sending_when_storage_is_none ()