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


#import "../common/helper.mligo" "Helper"
#import "../common/ticket/chusai_ticket.mligo" "Ticket"
#import "../common/mint_sc_entrypoints.mligo" "Entrypoints"

(** Alias for entrypoint.  *)
type entrypoint = Entrypoints.t
(* FIXME: refer to Ligo Team

   It is (IMHO) a little bit weird that [type t = k] introduces
   all constructors into the local scope. For me, type aliases
   should not introduces more than the fresh defined alias
   in the scope. In OCaml, there is a shortcut for reintroducing
   value constructors in the scope, using this kind of syntax:

  ```
  type t = O.t =
    | Foo of int
    | Bar of float
  ```
  (supposing that [O.t] is defined as [type t = Foo of int | Bar of int]). *)

(** Initiation of the state. It should be initiated at the origination of
    the contract. A configuration is a [storage] that does not change. A constant
    [storage]. It is only used for accepting one particular kind of payload and
    a minimal amount of XTZ for the participation. *)
type configuration = {
  fixed_payload: Ticket.payload
; minimal_accepted_quantity: tez
}

(** Create a ticket with the [fixed_payload], the function will fail if the
    quantity si lower than the [minimal_accepted_quantity] *)
let create_ticket (config : configuration) (quantity: tez) : Ticket.t =
  if quantity >= config.minimal_accepted_quantity
  then
    let quantity_nat = Helper.tez_to_nat quantity in
    let payload = config.fixed_payload in
    Ticket.create_ticket payload quantity_nat
  else
    failwith "mint_sc: the given quantity is lower than the minimal accepted amount."

(** [mint caller payload qty] creates a Chusai_ticket with the given payload and
    the given quantity and resend-it (as a transaction) to the caller. *)
let mint
     (config: configuration)
     (caller_contract: Ticket.t contract)
     (quantity: tez) : operation list =
  let created_ticket = create_ticket config quantity in
  let operation = Tezos.transaction created_ticket 0tez caller_contract in
  [ operation ]


(** Main is the entrypoint of [min_sc] contract. *)
let main
    (action, config: entrypoint * configuration) : operation list * configuration
  =
  let operations =
    match action with
    | Mint_mint caller_contract ->
      let quantity = Tezos.amount in
      mint config caller_contract quantity
  in (operations, config)
(** FIXME: refer to Ligo Team

    We cannot pattern-match on distant value constructor. ie:
    ```
    let operations =
      match actions with
      | Endpoints.Mint ... -> ...
    in (operations, x)
    ```
   Will fail with this error:

   ```
   Ill-formed pattern matching.
   At this point, if the pattern is complete, an arrow '->' is expected,
   followed by an expression.
   ```

   Which strange since the pattern seems well formed. *)
