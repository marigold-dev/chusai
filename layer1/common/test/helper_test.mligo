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

#import "../../common/helper.mligo" "Helper"
#import "../../common/test_framework/main.mligo" "Unit_test"

let _test_conversion_mutez =
  Unit_test.Make.a_test
    "tez_to_nat"
    "when it takes [1mutez], it should returns [1nat]"
    (fun (_log_level:Unit_test.Logger.level) ->
      let expected = 1n in
      let computed = Helper.tez_to_nat 1mutez in
      Unit_test.Assert.is_true
        "1mutez should equal 1nat"
        (expected = computed))

let _test_conversion_42tez =
  Unit_test.Make.a_test
    "tez_to_nat"
    "when it takes [42tez], it should returns [42000000n]"
    (fun (_log_level:Unit_test.Logger.level) ->
      let expected = 42000000n in
      let computed = Helper.tez_to_nat 42tez in
      Unit_test.Assert.is_true
        "42tez should equal 42000000nat"
        (expected = computed))

let _test_conversion_nat =
  Unit_test.Make.a_test
    "nat_to_tez"
    "when it takes [1nat], it should returns [1mutez]"
    (fun (_log_level:Unit_test.Logger.level) ->
      let expected = 1mutez in
      let computed = Helper.nat_to_tez 1n in
      Unit_test.Assert.is_true
        "1mutez should equal 1nat"
        (expected = computed))

let _test_conversion_42000000nat =
  Unit_test.Make.a_test
    "nat_to_tez"
    "when it takes [42000000nat], it should returns [42tez]"
    (fun (_log_level:Unit_test.Logger.level) ->
      let expected = 42tez in
      let computed = Helper.nat_to_tez 42000000n in
      Unit_test.Assert.is_true
        "1mutez should equal 1nat"
        (expected = computed))

let suite =
  Unit_test.Make.a_suite
    "Common.Helper"
    "Test suite related to Common Helpers"
    [ _test_conversion_mutez
    ; _test_conversion_42tez
    ; _test_conversion_nat
    ; _test_conversion_42000000nat ]
