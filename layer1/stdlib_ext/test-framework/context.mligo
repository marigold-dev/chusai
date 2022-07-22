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

(** Define a context for performing test. *)

(** Define an actor *)
type actor = {
  name: string
; starting_amount: tez
; address: address
}

let init_with_aux (reset : nat -> tez list -> unit) (actors: (string * tez) list) : actor list =
  let account_number = List.size actors in
  let default_amounts =
    List.map (fun (_name, value: string * tez) -> value) actors
  in
  let () = reset account_number default_amounts in
  let init : nat * (actor list) = (0n, []) in
  let _, x =
    List.fold_left (
      fun ((i, xs), (name, value) : (nat * actor list) * (string * tez)) ->
        let address = Test.nth_bootstrap_account (int i) in
        let actor: actor = {
          name = name
        ; starting_amount = value
        ; address = address
        }
        in ((i + 1n), actor :: xs)
  ) init actors in x

(** Initialize a list of actors, starting the chain at a particular [timestamp]. *)
let init_at_with (timestamp : timestamp) (actors: (string * tez) list) : actor list =
  let reset = Test.reset_state_at timestamp in
  init_with_aux reset actors

(** Initialize a list of actors. *)
let init_with (actors: (string * tez) list) : actor list =
  init_with_aux Test.reset_state actors

let init_default_aux (init_function: ((string * tez) list) -> actor list) : actor * (actor * actor * actor) =
  let alice = "Alice", 4000000tez in
  let bob = "Bob", 2000000tez in
  let carol = "Carol", 8000000tez in
  let operator = "Operator", 10000000000tez in
  let actors = init_function [alice; bob; carol; operator] in
  match actors with
  | [operator; carol; bob; alice] ->
    let () = Test.set_baker operator.address in
    operator, (alice, bob, carol)
  | _ -> failwith "An error occured."

(** Create a pair :
    - first member is an operator. Usually the account that initiates the rollup.
    - second member, a triplet with 3 accounts: alice, bob and carol. *)
let init_default () : actor * (actor * actor * actor) =
  init_default_aux init_with

(** Create a pair :
    - first member is an operator. Usually the account that initiates the rollup.
    - second member, a triplet with 3 accounts: alice, bob and carol. 
    - state is initialised at [timestamp]*)
let init_default_at (timestamp : timestamp) : actor * (actor * actor * actor) =
  let init_function = init_at_with timestamp in
  init_default_aux init_function

(** Execute an operation as a specific actor 
    ! BEWARE ! changes the source for subsequent actions too *)
let act_as (type a) (actor: actor) (handler: (unit -> a)) : a =
  let address = actor.address in
  let () = Test.set_source address in
  handler ()
