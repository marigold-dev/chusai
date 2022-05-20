#include "../src/wallet_sc.mligo"
#include "../test/unit/fakes.mligo"
#include "../../stdlib_ext/src/stdlibtestext.mligo"

let metric_wallet_gas
    (initial_mint_tez : tez)
    (initial_ticket_storage : chusai_ticket_storage) 
    (body : wallet_parameter contract -> test_exec_result list) : unit -> test_exec_result list =
    fun () ->
      let _, mint_address = TestExt.originate fake_mint_main unit initial_mint_tez in

      let bridge_initial_storage = {
        tickets = (None : chusai_ticket option);
      } in

      let _, bridge_address = TestExt.originate fake_bridge_main bridge_initial_storage 0tez in
      
      let wallet_initial_storage = {
        mint_address = mint_address;
        bridge_address = bridge_address;
        ticket_storage = initial_ticket_storage
      } in

      let wallet_contract, _ = TestExt.originate main wallet_initial_storage 0tez in
      body wallet_contract

let wallet_metrics : metric list =
  [ Metric.make 
      "mint_xtz_without_ticket_in_storage"
      ( metric_wallet_gas 
          0tez
          (None : chusai_ticket option)
          (fun (contr : wallet_parameter contract) -> [Test.transfer_to_contract contr Mint_xtz 10tez]));
    Metric.make 
      "mint_ticket_and_join_with_existing_ticket_in_storage"
      ( let initial_ticket = create_ticket 0x00 10n in
        metric_wallet_gas 
          0tez
          (Some initial_ticket)
          (fun (contr : wallet_parameter contract) -> [ Test.transfer_to_contract contr Mint_xtz 10tez]));
    Metric.make 
      "redeem_xtz"
      ( let initial_ticket = create_ticket 0x00 10n in
        metric_wallet_gas 
          10tez
          (Some initial_ticket) 
          (fun (contr : wallet_parameter contract) -> [Test.transfer_to_contract contr Redeem_xtz 0tez]));
    Metric.make 
      "send"
      ( let initial_ticket = create_ticket 0x00 10n in
        metric_wallet_gas 
          0tez
          (Some initial_ticket) 
          (fun (contr : wallet_parameter contract) -> [Test.transfer_to_contract contr Send 0tez]))
  ]

let test_metrics_main =
  List.iter 
    (fun (m : metric) -> Metric.run m) 
    wallet_metrics