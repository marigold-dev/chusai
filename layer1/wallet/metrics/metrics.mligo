#include "../src/wallet_sc.mligo"
#include "../test/unit/fakes.mligo"
#import "../../stdlib_ext/src/atomic_test.mligo" "Atom"
 
let metric_wallet_gas
    (initial_mint_tez : tez)
    (initial_ticket_storage : chusai_ticket_storage) 
    (body : wallet_parameter contract -> Atom.status) : unit -> Atom.status =
    fun () ->
      let taddress, _, _ = Test.originate fake_mint_main unit initial_mint_tez in
      let  mint_address = Tezos.address (Test.to_contract taddress) in

      let bridge_initial_storage = {
        tickets = (None : chusai_ticket option);
      } in

      let taddress, _, _ =  Test.originate fake_bridge_main bridge_initial_storage 0tez in
      let bridge_address = Tezos.address  (Test.to_contract taddress)in
      
      let wallet_initial_storage = {
        mint_address = mint_address;
        bridge_address = bridge_address;
        ticket_storage = initial_ticket_storage
      } in

      let taddress, _, _ = Test.originate main wallet_initial_storage 0tez in
      let wallet_contract = Test.to_contract taddress in
      body wallet_contract

let wallet_metrics : Atom.test_suite =
  Atom.make_suite
  "Wallet_sc metrics"
  [ Atom.make_test 
      "mint_xtz_without_ticket_in_storage"
      ""
      ( metric_wallet_gas 
          0tez
          (None : chusai_ticket option)
          (fun (contr : wallet_parameter contract) -> Atom.transfer_to_contract_ contr Mint_xtz 10tez))
  ; Atom.make_test   
      "mint_ticket_and_join_with_existing_ticket_in_storage"
      ""
      ( let initial_ticket = create_ticket 0x00 10n in
        metric_wallet_gas 
          0tez
          (Some initial_ticket)
          (fun (contr : wallet_parameter contract) -> Atom.transfer_to_contract_  contr Mint_xtz 10tez))
  ; Atom.make_test 
      "redeem_xtz"
      ""
      ( let initial_ticket = create_ticket 0x00 10n in
        metric_wallet_gas 
          10tez
          (Some initial_ticket) 
          (fun (contr : wallet_parameter contract) -> Atom.transfer_to_contract_  contr Redeem_xtz 0tez))
  ; Atom.make_test  
      "send"
      ""
      ( let initial_ticket = create_ticket 0x00 10n in
        metric_wallet_gas 
          0tez
          (Some initial_ticket) 
          (fun (contr : wallet_parameter contract) -> Atom.transfer_to_contract_  contr Send 0tez))
  ]

let test_metrics_main =
  Atom.run_suites_metrics [ wallet_metrics ]