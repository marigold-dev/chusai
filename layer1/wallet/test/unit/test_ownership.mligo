#import "../../../commons/ticket/chusai_ticket.mligo" "Ticket"
#import "../../../mint/src/mint_sc.mligo" "Mint"
#import "../../../wallet/src/wallet_sc.mligo" "Wallet"
#import "../../../commons/mint_interface.mligo" "Mint_interface"
#import "../../../commons/wallet_interface.mligo" "Wallet_interface"
#import "../../../wallet/test/unit/tools.mligo" "Tools"

#import "/../../../stdlib_ext/src/unit_test.mligo" "Unit_test"

#include "../../../stdlib_ext/src/stdlibext.mligo"

type wallet_entrypoint = Wallet_interface.wallet_parameter
type wallet_state = Wallet_interface.wallet_storage
type mint_configuration = Mint.storage
type mint_entrypoint = Mint_interface.mint_parameter
type originated = Unit_test.originated

let originate_mint (configuration: mint_configuration) : (mint_entrypoint, mint_configuration) originated =
    Unit_test.originate Mint.main configuration 0tez

let originate_mint_with () : (mint_entrypoint, mint_configuration) originated =
    let default_config = {
        payload = 0x00
    ;   minimum_amount = 1tez
    } in
    originate_mint default_config

let originate_wallet
    (owner: address)
    (mint : address)
    (default_ticket : Ticket.t option) 
    () : (wallet_entrypoint, wallet_state) originated =
    let default_state : wallet_state = {
        owner_address = owner;
        mint_address = mint;
        bridge_address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address);
        ticket_storage = default_ticket
    } in
    Unit_test.originate Wallet.main default_state 0tez

let mint_ticket
    (previous: Unit_test.result)
    (wallet: (wallet_entrypoint, wallet_state) originated)
    (fund: tez)  
    () : Unit_test.result =
    Unit_test.transfer_to_contract
      previous
      wallet.originated_contract
      Mint_xtz
      fund

let redeem_ticket
    (previous: Unit_test.result)
    (wallet: (wallet_entrypoint, wallet_state) originated)
    (fund: tez)  
    () : Unit_test.result =
    Unit_test.transfer_to_contract
      previous
      wallet.originated_contract
      Redeem_xtz
      fund

let check_ticket_value
   (expected_payload: bytes)
   (expected_quantity: nat)
   (ticket: Ticket.t) : bool =
   let (_, (payload, quantity)), _ = Ticket.read_ticket ticket in
   (quantity = expected_quantity) && (expected_payload = payload)

let _test_wallet_origination =
  Unit_test.make_test
    "just a origination of a Wallet_sc"
    "When nothing is transfered the balance of the wallet contract should be 0tez"
    (fun () ->
      let (operator, participants) = Unit_test.init_default () in
      let (alice, bob, carol) = participants in

      let mint =
        Unit_test.act_as operator
          (originate_mint_with)
      in

      let wallet =
         Unit_test.act_as operator
           (originate_wallet
            alice.address
            mint.originated_address
            (None : Ticket.t option)
            )
      in
      let expected = 0tez in
      let computed = Test.get_balance wallet.originated_address in
      Unit_test.assert_ (expected = computed) "Balance should be 0tez" )


let _test_wallet_mint_10tez_with_invalid_owner =
  Unit_test.make_test
    "An actor originate a wallet, and an another try to mint with it"
    "When the source is not the owner it should raise an error"
    (fun () ->

      let (operator, participants) = Unit_test.init_default () in
      let (alice, bob, carol) = participants in

      let mint =
        Unit_test.act_as operator
          (originate_mint_with)
      in
      let wallet =
         Unit_test.act_as operator
           (originate_wallet
              alice.address
              mint.originated_address
              (None : Ticket.t option)
            )
      in
      let result =
        Unit_test.act_as bob
          (mint_ticket (Unit_test.start ()) wallet 10tez)
      in
      let wallet_storage = Test.get_storage wallet.originated_typed_address in
      let wallet_balance = Test.get_balance wallet.originated_address in
      let mint_balance = Test.get_balance mint.originated_address in
      Unit_test.and_list
        [ Unit_test.assert_rejected_with_error
            result
            (Test.compile_value "Wallet_sc: invalid owner for this wallet")
            "Should be rejected because invalid source"
        ; Unit_test.assert_
            (wallet_balance = 0tez)
            "Wallet balance should be 0tez"
        ; Unit_test.assert_
            (mint_balance = 0tez)
            "Mint balance should be 0tez"
          ]
    )


let _test_wallet_redeem_good_owner =
  Unit_test.make_test
    "An actor originate, mint, and redeem with his wallet"
    "Redeem without error (10tez)"
    (fun () ->

      let (operator, participants) = Unit_test.init_default () in
      let (alice, bob, carol) = participants in

      let mint =
        Unit_test.act_as operator
          (originate_mint_with )
      in

      let wallet =
         Unit_test.act_as operator
           (originate_wallet
              alice.address
              mint.originated_address
              (None : Ticket.t option)
            )
      in


      let minted =
        Unit_test.act_as alice
          (mint_ticket (Unit_test.start ()) wallet 10tez)
      in

      let redeemed =
        Unit_test.act_as alice
          (redeem_ticket minted wallet 0tez)
      in

      let wallet_storage = Test.get_storage wallet.originated_typed_address in
      let wallet_balance = Test.get_balance wallet.originated_address in
      let mint_balance = Test.get_balance mint.originated_address in
      let ticket_storage = wallet_storage.ticket_storage in

      Unit_test.and_list [
        minted
      ; redeemed
      ; Unit_test.assert_
          (OptionExt.is_none ticket_storage)
          "Wallet ticket should be null"
      ])


let suite =
  Unit_test.make_suite
    "Wallet_sc"
    "Test suite related the ownership concept of Wallet_sc"
    [ _test_wallet_origination
    ; _test_wallet_mint_10tez_with_invalid_owner
    ; _test_wallet_redeem_good_owner
    ]