(* MIT License

   Copyright (c) 2022 Marigold <contact@marigold.dev>

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


#import "../../common/ticket/chusai_ticket.mligo" "Ticket"
#import "helper.mligo" "Helper"
#import "../mint_sc.mligo" "Mint_sc"
#import "../../common/test_framework/main.mligo" "Unit_test"

let () =
  let account_number = 5n in
  let default_amounts : tez list = [] in
  Test.reset_state account_number default_amounts

let check_ticket_value
   (expected_payload: bytes)
   (expected_quantity: nat)
   (ticket: Ticket.t) : bool =
   let (_, (payload, quantity)), _ = Ticket.read_ticket ticket in
   (quantity = expected_quantity) && (expected_payload = payload)

let _test_wallet_origination =
  Unit_test.Make.a_test
    "originate Wallet_sc without mint interaction"
    "When nothing is transfered the balance of the wallet contract should be 0tez"
    (fun (log_level:Unit_test.Logger.level) ->
      let wallet = Helper.originate_wallet log_level (None: Ticket.t option) in
      let expected = 0tez in
      let computed = Test.get_balance wallet.originated_address in
      Unit_test.Assert.is_true "Balance should be 0tez" (expected = computed))

let _test_wallet_mint_0tez =
  Unit_test.Make.a_test
    "originate Wallet_sc sending 0tez"
    "When nothing is transfered the balance of the wallet contract should be 0tez"
    (fun (log_level:Unit_test.Logger.level) ->
      let wallet = Helper.originate_wallet log_level (None: Ticket.t option) in
      let mint = Helper.originate_mint_with log_level in
      let result = Helper.mint_ticket Unit_test.neutral mint wallet 0tez in
      let wallet_storage = Test.get_storage wallet.originated_typed_address in
      let stored_ticket = wallet_storage.stored_ticket in
      let wallet_balance = Test.get_balance wallet.originated_address in
      Unit_test.reduce
        [ Unit_test.Assert.expect_rejection_with_error
            "Should be rejected because 0tez"
            (Test.compile_value "wallet_sc: Amount of tez should be non-null")
            result
        ; Unit_test.Assert.is_none
            "No ticket should received"
            stored_ticket
        ; Unit_test.Assert.is_true
            "Wallet balance should be 0tez"
            (wallet_balance = 0tez) ]
    )

let _test_wallet_mint_10tez =
  Unit_test.Make.a_test
    "originate Wallet_sc sending 10tez"
    "When a valid amount of tez is sent, it should store a generate a proper ticket"
    (fun (log_level:Unit_test.Logger.level) ->
      let wallet = Helper.originate_wallet log_level (None: Ticket.t option) in
      let mint = Helper.originate_mint_with log_level in
      let result = Helper.mint_ticket Unit_test.neutral mint wallet 10tez in
      let wallet_storage = Test.get_storage wallet.originated_typed_address in
      let stored_ticket = wallet_storage.stored_ticket in
      let wallet_balance = Test.get_balance wallet.originated_address in
      let mint_balance = Test.get_balance mint.originated_address in
      Unit_test.reduce
        [ Unit_test.Assert.replace_message
            "Should be validated"
            result
        ; Unit_test.Assert.is_some_when
            "There should be some tickets"
            (check_ticket_value 0x00 10000000n)
            stored_ticket
        ; Unit_test.Assert.is_true
            "Wallet balance should be 0tez"
            (wallet_balance = 0tez)
        ; Unit_test.Assert.is_true
            "Mint balance should be 10tez"
            (mint_balance = 10tez) ]
    )

let _test_wallet_mint_10tez_to_existing_ticket =
  Unit_test.Make.a_test
    "originate Wallet_sc sending 10tez joined with an existing ticket"
    "When a valid amount of tez is sent, it should join to an existing ticket"
    (fun (log_level:Unit_test.Logger.level) ->
      let default_ticket = Ticket.create_ticket 0x00 15000000n in
      let wallet = Helper.originate_wallet log_level (Some default_ticket) in
      let mint = Helper.originate_mint_with log_level in
      let result = Helper.mint_ticket Unit_test.neutral mint wallet 10tez in
      let wallet_storage = Test.get_storage wallet.originated_typed_address in
      let stored_ticket = wallet_storage.stored_ticket in
      let wallet_balance = Test.get_balance wallet.originated_address in
      let mint_balance = Test.get_balance mint.originated_address in
      Unit_test.reduce
        [ Unit_test.Assert.replace_message
            "Should be validated"
            result
        ; Unit_test.Assert.is_some_when
            "There should be some tickets"
            (check_ticket_value 0x00 25000000n)
            stored_ticket
        ; Unit_test.Assert.is_true
            "Wallet balance should be 0tez"
            (wallet_balance = 0tez)
        ; Unit_test.Assert.is_true
            "Mint balance should be 10tez"
            (mint_balance = 10tez) ]
    )

let _test_wallet_mint_invalid_payload =
  Unit_test.Make.a_test
    "originate Wallet_sc sending ticket with invalid_payload"
    "When nothing is transfered the balance of the wallet contract should be 0tez"
    (fun (log_level:Unit_test.Logger.level) ->
      let default_ticket = Ticket.create_ticket 0x01 15000000n in
      let wallet = Helper.originate_wallet log_level (Some default_ticket) in
      let config : Mint_sc.configuration = {
        fixed_payload = 0x00
      ; minimal_accepted_quantity = 1tez
      }
      in
      let mint = Helper.originate_mint log_level config in
      let result = Helper.mint_ticket Unit_test.neutral mint wallet 1tez in
      let wallet_storage = Test.get_storage wallet.originated_typed_address in
      let stored_ticket = wallet_storage.stored_ticket in
      let wallet_balance = Test.get_balance wallet.originated_address in
      Unit_test.reduce
        [ Unit_test.Assert.expect_rejection_with_error
            "Should be rejected because 0tez"
            (Test.compile_value "wallet_sc: Ticket payload is invalid")
            result
        ; Unit_test.Assert.is_true
            "Wallet balance should be 0tez"
            (wallet_balance = 0tez) ]
    )



let suite =
  Unit_test.Make.a_suite
    "Wallet_sc"
    "Test suite related to Wallet_sc (the wallet smart contract)"
    [ _test_wallet_origination
    ; _test_wallet_mint_0tez
    ; _test_wallet_mint_10tez
    ; _test_wallet_mint_10tez_to_existing_ticket
    ; _test_wallet_mint_invalid_payload ]
