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

(** Some helpers for contract origination *)

(** A type that define an originated contract *)
type ('a, 'b) originated = {
  originated_typed_address : ('a, 'b) typed_address
; originated_contract : 'a contract
; originated_address : address
}

(** [originate main default_storage quantity] will originated
    a contract:
    - [main] : the main function of the contract
    - [storage] : the default value of the storage
    - [quantity] : the given amount of tez
 *)
let originate
    (type a b)
    (main: (a * b -> (operation list * b)))
    (storage: b)
    (quantity: tez) : (a, b) originated =
  let typed_address, _, _ = Test.originate main storage quantity in
  let contract = Test.to_contract typed_address in
  let address = Tezos.address contract in
  { originated_typed_address = typed_address
  ; originated_contract = contract
  ; originated_address = address }


(** [originate_from_file  "foo/bar/contract.mligo" "main" ["view1" ; "view2"] default_storage quantity] will originated
    a contract:
    - [file] : file containing the contract
    - [main] : the name of the main function of the contract
    - [views] : list of the names of the views
    - [storage] : the default value of the storage (as a ligo expression)
    - [quantity] : the given amount of tez
 *)
let originate_from_file 
    (type a b)
    (file: string)
    (main: string)
    (views : string list)
    (storage: b)
    (quantity: tez) : (a, b) originated =
    let michelson_storage = Test.compile_value storage in
    let address, _, _ = Test.originate_from_file file main views michelson_storage quantity in
    let typed_address : (a, b) typed_address = Test.cast_address address in    
    let contract = Test.to_contract typed_address in
    { originated_typed_address = typed_address
    ; originated_contract = contract
    ; originated_address = address }