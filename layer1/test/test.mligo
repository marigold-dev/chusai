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

#import "../common/test/helper_test.mligo" "Common_helper_test"
#import "../bootstrap/test/mint_sc_test.mligo" "Mint_sc_test"
#import "../bootstrap/test/wallet_sc_test.mligo" "Wallet_sc_test"
#import "../bridge/test/inbox_sc_test.mligo" "Inbox_sc_test"
#import "../common/test_framework/main.mligo" "Unit_test"

let log_level = Void

let suites = [
  Common_helper_test.suite
; Mint_sc_test.suite
; Wallet_sc_test.suite
; Inbox_sc_test.suite
]

let test = Unit_test.run log_level suites
