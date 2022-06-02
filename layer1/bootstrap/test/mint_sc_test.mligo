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
#import "../../common/test_framework/main.mligo" "Unit_test"

let () =
  let account_number = 5n in
  let default_amounts : tez list = [] in
  Test.reset_state account_number default_amounts

let _test_mint_origination =
  Unit_test.Make.a_test
    "originate Mint_sc without wallet interaction"
    "When nothing is transfered the balance of the mint contract should be 0tez"
    (fun (log_level:Unit_test.Logger.level) ->
      let mint = Helper.originate_mint_with log_level in
      let expected = 0tez in
      let computed = Test.get_balance mint.originated_address in
      Unit_test.Assert.is_true "Balance should be 0tez" (expected = computed))

let _test_mint_first_ticket =
  Unit_test.Make.a_test
    "originate Mint_sc with wallet interaction"
    "When a Ticket is minted (and send to the wallet) it should update the balance"
    (fun (log_level:Unit_test.Logger.level) ->
      let mint = Helper.originate_mint_with log_level in
      let () = Test.log mint in
      let wallet = Helper.originate_wallet log_level (None : Ticket.t option) in
      let result = Helper.mint_ticket Unit_test.neutral mint wallet 1tez in
      let () = Test.log wallet in
      let () = Test.log result in
      let wallet_balance = Test.get_balance wallet.originated_address in
      let mint_balance = Test.get_balance mint.originated_address in
      Unit_test.reduce
        [ Unit_test.Assert.replace_message
            "Mint transaction should be OK"
            result

        ; Unit_test.Assert.is_true
            "Wallet balance should be 0tez"
            (wallet_balance = 0tez)

        ; Unit_test.Assert.is_true
            "Mint balance should be 1tez"
            (mint_balance = 1tez)
        ]
    )

let _test_mint_fail_for_first_ticket =
  Unit_test.Make.a_test
    "originate Mint_sc with wallet interaction"
    "When a Ticket is minted (and send to the wallet) it should update the balance"
    (fun (log_level:Unit_test.Logger.level) ->
      let mint = Helper.originate_mint_with log_level in
      let wallet = Helper.originate_wallet log_level (None : Ticket.t option) in
      let result = Helper.mint_ticket Unit_test.neutral mint wallet 1mutez in
      let wallet_balance = Test.get_balance wallet.originated_address in
      let mint_balance = Test.get_balance mint.originated_address in
      let wallet_storage = Test.get_storage wallet.originated_typed_address in
      let stored_ticket = wallet_storage.stored_ticket in
      Unit_test.reduce
        [ Unit_test.Assert.expect_execution_rejected_at
            "Mint should be rejected because of to low amount"
            mint.originated_address
            result

        ; Unit_test.Assert.is_none
            "No ticket should received"
            stored_ticket

        ; Unit_test.Assert.is_true
            "Mint balance should be 0tez"
            (mint_balance = 0tez)
        ]
    )

let suite =
  Unit_test.Make.a_suite
    "Mint_sc"
    "Test suite related to Mint_sc (the mint smart contract)"
    [ _test_mint_origination
    ; _test_mint_first_ticket
    ; _test_mint_fail_for_first_ticket ]
