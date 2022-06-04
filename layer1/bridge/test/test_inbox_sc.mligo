#import "../../commons/ticket_api_workaround.mligo" "Ticket"
#import "../../mint/src/mint_sc.mligo" "Mint"
#import "../../wallet/src/wallet_sc.mligo" "Wallet"
#import "../src/inbox_sc.mligo" "Inbox"
#import "../../commons/inbox_interface.mligo" "Inbox_interface"
#import "../../commons/mint_interface.mligo" "Mint_interface"
#import "../../commons/wallet_interface.mligo" "Wallet_interface"
#import "../../stdlib_ext/src/atomic_test.mligo" "Atom"
#import "../../wallet/test/unit/tools.mligo" "Tools"


let debug = false
let log_ (type a) (msg : a) = if debug then Test.log msg

type ('a, 'b) originated = {
  originated_typed_address : ('a, 'b) typed_address
; originated_contract : 'a contract
; originated_address : address
}

type inbox_state = Inbox.state
type inbox_entrypoint = Inbox_interface.entrypoint
type message = Inbox_interface.message
type wallet_entrypoint = Wallet_interface.wallet_parameter
type wallet_state = Wallet_interface.wallet_storage
type mint_configuration = Mint.storage
type mint_entrypoint = Mint_interface.mint_parameter

let empty_state : inbox_state = {
    rollup_level = 0n
;   ticket = None
;   messages = (Big_map.empty : (nat, message list) big_map)
;   fixed_payload = 0x00
}

let originate_full (type a b) (main : a * b -> operation list * b ) (storage : b) (bal : tez) (log : string) : (a, b) originated = 
  let my_taddr, _, _ = Test.originate main storage bal in
  let my_contr = Test.to_contract my_taddr in
  let my_addr = Tezos.address my_contr in
  let _ = log_ (log, storage, bal, my_addr) in
  {originated_typed_address = my_taddr ; originated_contract = my_contr ; originated_address = my_addr}

let originate_inbox () : (inbox_entrypoint, inbox_state) originated =
  originate_full Inbox.main empty_state 0tez "Originated Inbox_sc"

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

let _test_deposit () =
  begin
    log_ "test deposit";
    (**The wallets needs mint and rollup (inbox) addresses to be originate**)
    let mint = originate_mint_with () in
    let rollup = originate_inbox () in

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

let suite = Atom.make_suite
"Bridge_sc: test suite of Bridge sc"
[
  Atom.make_test "deposit" "A entire scenario with 3 wallet which ask for tickets and deposit them" _test_deposit
]