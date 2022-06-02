(* MIT License

   Copyright (C) 2022 Marigold <contact@marigold.dev>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
   the Software, and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

#import "../mint_sc.mligo" "Mint_sc"
#import "../wallet_sc.mligo" "Wallet_sc"
#import "../../common/wallet_sc_entrypoints.mligo" "Wallet_entrypoints"
#import "../../common/mint_sc_entrypoints.mligo" "Mint_entrypoints"
#import "../../common/ticket/chusai_ticket.mligo" "Ticket"
#import "../../common/test_framework/main.mligo" "Unit_test"

type originated = Unit_test.Contract.originated
(** FIXME: Refer to Ligo team
    It seems extremely weird to not transpose type parameters. *)

type mint_configuration = Mint_sc.configuration
type mint_entrypoint = Mint_entrypoints.t
type wallet_entrypoint = Wallet_entrypoints.t
type wallet_state = Wallet_sc.state

(** Originate [Mint_sc]. *)
let originate_mint
    (log_level: Unit_test.Logger.level)
    (configuration: mint_configuration) : (mint_entrypoint, mint_configuration) originated =
  Unit_test.Contract.originate
    log_level
    "Originated [Mint_sc]"
    Mint_sc.main
    configuration
    0tez

(** Originated [Mint_sc] with a default config value *)
let originate_mint_with
    (log_level: Unit_test.Logger.level) : (mint_entrypoint, mint_configuration) originated =
  let default_config = {
    fixed_payload = 0x00
  ;  minimal_accepted_quantity = 1tez
  } in
  originate_mint log_level default_config

(** Originated [Wallet_sc]. *)
let originate_wallet
    (log_level: Unit_test.Logger.level)
    (default_ticket: Ticket.t option) : (wallet_entrypoint, wallet_state) originated =
  let default_state : wallet_state = { stored_ticket = default_ticket } in
  Unit_test.Contract.originate
    log_level
    "Originated [Wallet_sc]"
    Wallet_sc.main
    default_state
    0tez

(** Request Mint from Wallet_sc to Mint_sc *)
let mint_ticket
    (previous: Unit_test.test_result)
    (mint: (mint_entrypoint, mint_configuration) originated)
    (wallet: (wallet_entrypoint, wallet_state) originated)
    (fund: tez) : Unit_test.test_result =
  Unit_test.transfer_to_contract
    previous
    wallet.originated_contract
    (Wallet_request_mint mint.originated_address)
    fund
