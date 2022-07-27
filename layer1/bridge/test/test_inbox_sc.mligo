#import "../../commons/ticket/chusai_ticket.mligo" "Ticket"
#import "../../mint/src/mint_sc.mligo" "Mint"
#import "../../wallet/src/wallet_sc.mligo" "Wallet"
#import "../src/inbox_sc.mligo" "Inbox"
#import "../../commons/mint_interface.mligo" "Mint_interface"
#import "../../commons/wallet_interface.mligo" "Wallet_interface"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#import "../../wallet/test/unit/tools.mligo" "Tools"

#include "../../stdlib_ext/src/stdlibext.mligo"
#include "../../stdlib_ext/src/originate_utils.mligo"
#include "../../commons/inbox_interface.mligo"

type inbox_state = Inbox.state
type inbox_entrypoint = entrypoint
type message = message
type wallet_entrypoint = Wallet_interface.wallet_parameter
type wallet_state = Wallet_interface.wallet_storage
type mint_configuration = Mint.storage
type mint_entrypoint = Mint_interface.mint_parameter


let empty_state (mint : address) : inbox_state = {
    max_inbox_level = 0n
;   ticket = None
;   fixed_ticket_key = {mint_address= mint; payload= Tools.dummy_payload}
;   inboxes = (Big_map.empty : Inbox.inboxes)
}

(**same payload but different ticketer**)
let empty_state2 : inbox_state = {
    max_inbox_level = 0n
;   ticket = None
;   fixed_ticket_key = {mint_address= ("tz1fVd2fmqYy1TagTo4Pahgad2n3n8GzUX1N" : address); payload= Tools.dummy_payload}
;   inboxes = (Big_map.empty : Inbox.inboxes)
}

let zero_ticket : Ticket.t = Ticket.create_ticket Tools.dummy_address 0x00 0n

let originate_inbox_with_state (state : inbox_state) : (inbox_entrypoint, inbox_state) originated =
  originate_full Inbox.main state 0tez "Originated Inbox_sc"

let originate_mint (configuration: mint_configuration) : (mint_entrypoint, mint_configuration) originated =
    originate_full Mint.main configuration 0tez "Originated Mint_sc" 

let originate_mint_with () : (mint_entrypoint, mint_configuration) originated =
    let default_config = {
        payload = 0x00
    ;   minimum_amount = 1tez
    } in
    originate_mint default_config

let originate_wallet
    (mint : address)
    (bridge : address)
    (default_ticket : Ticket.t option) : (wallet_entrypoint, wallet_state) originated =
    let default_state : wallet_state = {
        owner_address = Tezos.self_address;
        mint_address = mint;
        bridge_address = bridge;
        ticket_storage = default_ticket
    } in
    originate_full Wallet.main default_state 0tez "Originate Wallet_sc"

let mint_ticket
    (previous: Unit.result)
    (wallet: (wallet_entrypoint, wallet_state) originated)
    (fund: tez) : Unit.result =
    Unit.transfer_to_contract
      previous
      wallet.originated_contract
      Mint_xtz
      fund

let deposit_ticket
    (previous: Unit.result)
    (wallet: (wallet_entrypoint,wallet_state) originated) : Unit.result =
    Unit.transfer_to_contract 
      previous 
      wallet.originated_contract
      Send
      0tez

let compute_total_balance (inbox: (inbox_entrypoint,inbox_state) originated) : nat = 
    let state = Test.get_storage inbox.originated_typed_address in
    let map = state.inboxes in
    let current = Big_map.find_opt 0n map in
    match current with
    | None -> 0n
    | Some inboxes ->
      List.fold_left (fun (acc, message: nat * message) ->
        match message with
        | Deposit {owner; quantity} -> acc + quantity
        | _ -> acc
      ) 0n inboxes

let empty_ticket () : Ticket.t option = None

let _test_success_deposit () =
  begin
    log_ "a successful scenario test for deposit";
    (**The wallets needs mint and rollup (inbox) addresses to be originate**)
    let mint = originate_mint_with () in
    let rollup = originate_inbox_with_state (empty_state mint.originated_address) in

    (** Wallets origination **)
    let gon = originate_wallet mint.originated_address rollup.originated_address (empty_ticket ()) in
    let kirua = originate_wallet mint.originated_address rollup.originated_address (empty_ticket ()) in
    let leolio = originate_wallet mint.originated_address rollup.originated_address (empty_ticket ()) in

    (** Mint tickets **)
    let gon_mint_result = mint_ticket (Unit.start ()) gon 10tez in
    let kirua_mint_result = mint_ticket gon_mint_result kirua 15tez in
    let leolio_mint_result = mint_ticket kirua_mint_result leolio 100tez in

    (**Deposit the tickets**)
    let gon_deposit_result = deposit_ticket leolio_mint_result gon in
    let kirua_deposit_result = deposit_ticket gon_deposit_result kirua in
    let leolio_deposit_result = deposit_ticket kirua_deposit_result leolio in


    let inbox_storage = Test.get_storage rollup.originated_typed_address in
    let inboxes = inbox_storage.inboxes in
    let opt_msgs = Big_map.find_opt 0n inboxes in
    let default_msg = [Deposit {owner = Tools.dummy_address; quantity = 0n}] in
    let msgs = OptionExt.default opt_msgs default_msg in

    let inbox_ticket = OptionExt.default inbox_storage.ticket zero_ticket in
    let ((_,(_,inbox_ticket_quantity)),_) = Ticket.read_ticket inbox_ticket in

    let gon_msg = Deposit {owner = gon.originated_address; quantity = 10000000n} in
    let kirua_msg = Deposit {owner = kirua.originated_address; quantity = 15000000n} in
    let leolio_msg = Deposit {owner = leolio.originated_address; quantity = 100000000n} in

    Unit.and_list 
    [  leolio_deposit_result
    ;  Unit.assert_ (OptionExt.is_some inbox_storage.ticket) "the rollup storage must contain a ticket after deposits"
    ;  Unit.assert_ ((compute_total_balance rollup) = inbox_ticket_quantity) "The sum of all the quantities stored on messages must be equal to the inbox ticket amount"
    ;  Unit.assert_ (msgs <> default_msg) "Messages list must be not empty"
    ;  Unit.assert_ (msgs = [leolio_msg;kirua_msg;gon_msg] ) "Messages list must be equal to a list with the 3 messages"
    ;  Unit.assert_ ((compute_total_balance rollup) = 125000000n) "After the 3 deposits, the balance should be equal to 125tez"
    ]
  end

let _test_fail_payload_deposit () =
  begin
    log_ "_test_fail_payload_deposit";
    let mint = originate_mint_with () in
    let rollup = originate_inbox_with_state (empty_state mint.originated_address) in
    let gon = originate_wallet mint.originated_address rollup.originated_address (empty_ticket ()) in

    (** Same ticketer because the empty state used at the rollup origination use dummy_address, and , 
       the create_ticket use here is from the dummy implementation which use the same dummy address as a ticketer.
       but we have different payload here**)
    let jhon = originate_wallet mint.originated_address rollup.originated_address (Some (Ticket.create_ticket mint.originated_address 0x01 10n)) in
    let gon_mint_result = mint_ticket (Unit.start ()) gon 10tez in

    let gon_deposit_result = deposit_ticket gon_mint_result gon in
    let jhon_deposit_result = deposit_ticket gon_deposit_result jhon in

    Unit.and_list
    [ Unit.assert_rejected_with_error jhon_deposit_result (Test.compile_value "Ticket key is invalid") "should be false because of a different payload"
    ]
  end

let _test_fail_ticketer_deposit () =
  begin
    log_ "_test_fail_ticketer_deposit";
    let mint = originate_mint_with () in
    let rollup = originate_inbox_with_state empty_state2 in

    (**Same payload but different ticketer thanks to empty_state2**)
    let jhon = originate_wallet mint.originated_address rollup.originated_address (Some (Ticket.create_ticket mint.originated_address 0x00 10n)) in
    let jhon_deposit_result = deposit_ticket (Unit.start ()) jhon in

    Unit.and_list
    [ Unit.assert_rejected_with_error jhon_deposit_result (Test.compile_value "Ticket key is invalid") "should be false because of a different ticketer"
    ]
  end

let _test_fail_entire_key_deposit () =
  begin
    log_ "_test_fail_entire_key_deposit";
    let mint = originate_mint_with () in
    let rollup = originate_inbox_with_state empty_state2 in

    (**different ticketer and payload**)
    let jhon = originate_wallet mint.originated_address rollup.originated_address (Some (Ticket.create_ticket mint.originated_address 0x01 10n)) in
    let jhon_deposit_result = deposit_ticket (Unit.start ()) jhon in

    Unit.and_list
    [ Unit.assert_rejected_with_error jhon_deposit_result (Test.compile_value "Ticket key is invalid") "should be false because of a different ticketer and payload"
    ]
  end

let _test_fail_0ticket_deposit () =
  begin
    log_ "a successful scenario test for deposit";

    (**The wallets needs mint and rollup (inbox) addresses to be originate**)
    let mint = originate_mint_with () in
    let rollup = originate_inbox_with_state (empty_state mint.originated_address) in

    (** Wallets origination **)
    let gon = originate_wallet mint.originated_address rollup.originated_address (empty_ticket ()) in

    (** Mint tickets **)
    let gon_mint_result = mint_ticket (Unit.start ()) gon 0tez in

    (**Deposit the tickets**)
    let gon_deposit_result = deposit_ticket gon_mint_result gon in

    Unit.and_list
    [  Unit.assert_rejected_at gon_deposit_result gon.originated_address "should have refused to deposit because the quantity < minimum amount"
    ] 
  end

let _test_simple_transaction_message () =
  begin
    log_ "test transaction message";

    (* setup *)
    let operator, actors = Unit.init_default () in
    let alice, bob, _ = actors in
    let mint = originate_mint_with () in

    let init_storage = (empty_state mint.originated_address) in
    let originate_inbox_sc () = Unit.originate Inbox.main init_storage 0tez in
    let inbox_sc = Unit.act_as operator originate_inbox_sc in

    (* perform *)
    let inbox_tx () = Unit.transfer_to_contract_ inbox_sc.originated_contract (Inbox_transaction { destination = alice.address; quantity = 1n; }) 0tez in

    (* check *)
    let result = Unit.act_as bob inbox_tx in
    let inbox_storage = Test.get_storage inbox_sc.originated_typed_address in
    let inboxes = inbox_storage.inboxes in
    let opt_msgs = Big_map.find_opt 0n inboxes in
    let msgs = OptionExt.default opt_msgs ([] : message list) in
    let expected_bob_message = Transaction { source = bob.address; destination = alice.address; quantity = 1n; arg = (None : bytes option) } in

    Unit.and_list
    [ result
    ; Unit.assert_ (msgs = [expected_bob_message]) "Messages list must have bob's message"
    ]
  end

let suite = Unit.make_suite
"Bridge_sc"
"Test suite of Bridge sc"
[
  Unit.make_test "successful deposit" "A entire scenario with 3 wallet which ask for tickets and deposit them" _test_success_deposit
; Unit.make_test "failure test deposit 1" "A fail test with a deposit with a different payload than the ones fixed at inbox originattion" _test_fail_payload_deposit
; Unit.make_test "failure test deposit 2" "A fail test with a deposit with a different ticketer than the ones fixed at inbox originattion" _test_fail_ticketer_deposit
; Unit.make_test "failure test deposit 3" "A fail test with a deposit with a different ticketer and payload than the ones fixed at inbox originattion" _test_fail_entire_key_deposit
; Unit.make_test "should reject deposit" "A test which verify that the 0-value ticket deposit is rejected" _test_fail_0ticket_deposit
; Unit.make_test "successful make a transaction" "test to make a transaction message" _test_simple_transaction_message
]
