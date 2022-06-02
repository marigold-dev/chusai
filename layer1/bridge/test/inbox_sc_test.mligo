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


#import "helper.mligo" "Helper"
#import "../../common/ticket/chusai_ticket.mligo" "Ticket"
#import "../../bootstrap/test/helper.mligo" "Helper_bootstrap"
#import "../../common/test_framework/main.mligo" "Unit_test"


let () =
  let account_number = 5n in
  let default_amounts : tez list = [] in
  Test.reset_state account_number default_amounts

let empty_ticket () : Ticket.t option = None

let _test_deposit =
  Unit_test.Make.a_test
    "complete scenario between multiple participant"
    "Alice, Bob and OCamlboy made some deposits"
    (fun (log_level:Unit_test.Logger.level) ->
      let mint = Helper_bootstrap.originate_mint_with log_level in
      let alice = Helper_bootstrap.originate_wallet log_level (empty_ticket ()) in
      let bob = Helper_bootstrap.originate_wallet log_level (empty_ticket ()) in
      let ocaml_boy = Helper_bootstrap.originate_wallet log_level (empty_ticket ()) in
      let rollup = Helper.originate_inbox log_level in
      let alice_mint = Helper_bootstrap.mint_ticket Unit_test.neutral mint alice 10tez in
      let bob_mint = Helper_bootstrap.mint_ticket alice_mint mint bob 15tez in
      let ocaml_boy_mint = Helper_bootstrap.mint_ticket bob_mint mint ocaml_boy 100tez in
      let alice_deposit = Helper.deposit_ticket ocaml_boy_mint alice rollup in
      let bob_deposit = Helper.deposit_ticket alice_deposit bob rollup in
      let ocaml_boy_deposit = Helper.deposit_ticket bob_deposit ocaml_boy rollup in
      Unit_test.reduce
        [ ocaml_boy_deposit
        ; Unit_test.Assert.is_true
            "The ledger should be equal to 10n"
            (Helper.compute_total_balance rollup = 125000000n)
        ]
    )

let suite =
  Unit_test.Make.a_suite
    "Inbox_sc"
    "Test suite related to Inbox_sc (the Inbox smart contract on the bridge)"
    [ _test_deposit ]
