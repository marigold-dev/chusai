#import "../../commons/ticket_api_workaround.mligo" "Ticket"
#import "../../mint/src/mint_sc.mligo" "Mint"
#import "../../wallet/src/wallet_sc.mligo" "Wallet"
#import "../src/inbox_sc.mligo" "Inbox"
#import "../../commons/inbox_interface.mligo" "Inbox_interface"
#import "../../commons/mint_interface.mligo" "Mint_interface"
#import "../../commons/wallet_interface.mligo" "Wallet_interface"
#import "../../stdlib_ext/src/atomic_test.mligo" "Atom"
#import "../../wallet/test/unit/tools.mligo" "Tools"

#include "../../stdlib_ext/src/originate_utils.mligo"

type inbox_state = Inbox.state
type inbox_entrypoint = Inbox_interface.entrypoint
type message = Inbox_interface.message
type wallet_entrypoint = Wallet_interface.wallet_parameter
type wallet_state = Wallet_interface.wallet_storage
type mint_configuration = Mint.storage
type mint_entrypoint = Mint_interface.mint_parameter



(**
autres scenarios de tests

  1/ deposit raté parce que ticketer different ou payload différent ou les deux différent
  2/ essayer de deposer des tickets de valeur 0
  3/ faire 3 déposit avec deux au niveau n et le dernier au niveau n+1

**)


let empty_state : inbox_state = {
    rollup_level = 0n
;   ticket = None
;   fixed_ticket_key = {ticketer= Tools.dummy_address; payload= Tools.dummy_payload}
;   messages = (Big_map.empty : (nat, message list) big_map)
}

(**same payload but different ticketer**)
let empty_state2 : inbox_state = {
    rollup_level = 0n
;   ticket = None
;   fixed_ticket_key = {ticketer= ("tz1fVd2fmqYy1TagTo4Pahgad2n3n8GzUX1N" : address); payload= Tools.dummy_payload}
;   messages = (Big_map.empty : (nat, message list) big_map)
}

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
    (default_ticket : Ticket.chusai_ticket option) : (wallet_entrypoint, wallet_state) originated =
    let default_state : wallet_state = {
        mint_address = mint;
        bridge_address = bridge;
        ticket_storage = default_ticket
    } in
    originate_full Wallet.main default_state 0tez "Originate Wallet_sc"

let mint_ticket
    (previous: Atom.status)
    (wallet: (wallet_entrypoint, wallet_state) originated)
    (fund: tez) : Atom.status =
    Atom.transfer_to_contract
      previous
      wallet.originated_contract
      Mint_xtz
      fund

let deposit_ticket
    (previous: Atom.status)
    (wallet: (wallet_entrypoint,wallet_state) originated) : Atom.status =
    Atom.transfer_to_contract 
      previous 
      wallet.originated_contract
      Send
      0tez

let compute_total_balance (inbox: (inbox_entrypoint,inbox_state) originated) : nat = 
    let state = Test.get_storage inbox.originated_typed_address in
    let map = state.messages in
    let current = Big_map.find_opt 0n map in
    match current with
    | None -> 0n
    | Some messages ->
      List.fold_left (fun (acc, message: nat * message) ->
        match message with
        | Deposit {owner; quantity} -> acc + quantity
      ) 0n messages

let empty_ticket () : Ticket.chusai_ticket option = None

let _test_success_deposit () =
  begin
    log_ "a successful scenario test for deposit";
    (**The wallets needs mint and rollup (inbox) addresses to be originate**)
    let mint = originate_mint_with () in
    let rollup = originate_inbox_with_state empty_state in

    (** Wallets origination **)
    let gon = originate_wallet mint.originated_address rollup.originated_address (empty_ticket ()) in
    let kirua = originate_wallet mint.originated_address rollup.originated_address (empty_ticket ()) in
    let leolio = originate_wallet mint.originated_address rollup.originated_address (empty_ticket ()) in

    (** Mint tickets **)
    let gon_mint_result = mint_ticket Atom.start gon 10tez in
    let kirua_mint_result = mint_ticket gon_mint_result kirua 15tez in
    let leolio_mint_result = mint_ticket kirua_mint_result leolio 100tez in

    (**Deposit the tickets**)
    let gon_deposit_result = deposit_ticket leolio_mint_result gon in
    let kirua_deposit_result = deposit_ticket gon_deposit_result kirua in
    let leolio_deposit_result = deposit_ticket kirua_deposit_result leolio in

    Atom.and_list 
    [  leolio_deposit_result
    ;  Atom.assert_ ((compute_total_balance rollup) = 125000000n) "After the 3 deposits, the balance should be equal to 125tez"
    ] 
  end

(**let _test_success_level_deposit () = **)

let _test_fail_key_deposit1 () =
  begin
    log_ "_test_fail_key_deposit1";
    let mint = originate_mint_with () in
    let rollup = originate_inbox_with_state empty_state in
    let gon = originate_wallet mint.originated_address rollup.originated_address (empty_ticket ()) in

    (** Same ticketer because the empty state used at the rollup origination use dummy_address, and , 
       the create_ticket use here is from the dummy implementation which use the same dummy address as a ticketer.
       but we have different payload here**)
    let jhon = originate_wallet mint.originated_address rollup.originated_address (Some (Ticket.create_ticket 0x01 10n)) in
    let gon_mint_result = mint_ticket Atom.start gon 10tez in

    let gon_deposit_result = deposit_ticket gon_mint_result gon in
    let jhon_deposit_result = deposit_ticket gon_deposit_result jhon in

    Atom.and_list
    [ Atom.assert_is_ok jhon_deposit_result "should be false because of a different payload"
    ]
  end

let _test_fail_key_deposit2 () =
  begin
    log_ "_test_fail_key_deposit2";
    let mint = originate_mint_with () in
    let rollup = originate_inbox_with_state empty_state2 in

    (**Same payload but different ticketer thanks to empty_state2**)
    let jhon = originate_wallet mint.originated_address rollup.originated_address (Some (Ticket.create_ticket 0x00 10n)) in
    let jhon_deposit_result = deposit_ticket Atom.start jhon in

    Atom.and_list
    [ Atom.assert_is_ok jhon_deposit_result "should be false because of a different ticketer"
    ]
  end

let _test_fail_key_deposit3 () =
  begin
    log_ "_test_fail_key_deposit3";
    let mint = originate_mint_with () in
    let rollup = originate_inbox_with_state empty_state2 in

    (**different ticketer and payload**)
    let jhon = originate_wallet mint.originated_address rollup.originated_address (Some (Ticket.create_ticket 0x01 10n)) in
    let jhon_deposit_result = deposit_ticket Atom.start jhon in

    Atom.and_list
    [ Atom.assert_is_ok jhon_deposit_result "should be false because of a different ticketer and payload"
    ]
  end

let _test_fail_0ticket_deposit () =
  begin
    log_ "a successful scenario test for deposit";
    (**The wallets needs mint and rollup (inbox) addresses to be originate**)
    let mint = originate_mint_with () in
    let rollup = originate_inbox_with_state empty_state in

    (** Wallets origination **)
    let gon = originate_wallet mint.originated_address rollup.originated_address (empty_ticket ()) in

    (** Mint tickets **)
    let gon_mint_result = mint_ticket Atom.start gon 0tez in

    (**Deposit the tickets**)
    let gon_deposit_result = deposit_ticket gon_mint_result gon in

    Atom.and_list 
    [  Atom.assert_rejected_at gon_deposit_result gon.originated_address "should have refused to deposit because the quantity < minimum amount"
    ] 
  end


let suite = Atom.make_suite
"Bridge_sc: test suite of Bridge sc"
[
  Atom.make_test "successful deposit" "A entire scenario with 3 wallet which ask for tickets and deposit them" _test_success_deposit
; Atom.make_test "failure deposit 1" "A fail test with a deposit with a different payload than the ones fixed at inbox originattion" _test_fail_key_deposit1
; Atom.make_test "failure deposit 2" "A fail test with a deposit with a different ticketer than the ones fixed at inbox originattion" _test_fail_key_deposit2
; Atom.make_test "failure deposit 3" "A fail test with a deposit with a different ticketer and payload than the ones fixed at inbox originattion" _test_fail_key_deposit3
; Atom.make_test "failure deposit 4" "A fail test with a deposit with a 0-value ticket" _test_fail_0ticket_deposit
]