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

(** A dummy implementation of tickets (which currently cause a bug in the
    execution of unit tests).
    Ref: https://gitlab.com/ligolang/ligo/-/issues/1402 *)

(** Aliases for allowing switching between real and dummy implementation. *)
type payload = bytes

(** Characterize a [Dummy_ticket]. *)
type t =
  Dummy_ticket of {
    payload: payload
  ; quantity: nat
  }

(** When a ticket has been read. *)
type been_read = (address * (payload * nat)) * t

(** When a ticket has beed splitted. *)
type splitted = (t * t) option

(** [Dummy_ticket.create_ticket payload qty]
    Create a [Dummy_ticket]. *)
let create_ticket (payload: bytes) (quantity: nat) =
  Dummy_ticket {
    payload = payload
  ; quantity = quantity
  }

(** [Dummy_ticket.read_ticket a_dummy_ticket]
    Read a [Dummy_ticket]. *)
let read_ticket (ticket: t) : been_read =
  match ticket with
  | Dummy_ticket { payload = payload; quantity = value } ->
    let random_address =
       ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
    in ((random_address, (payload, value)), ticket)


(** [Dummy_ticket.join_tickets left_ticket right_ticket]
    Join two [Dummy_ticket] of the same kind. *)
let join_tickets (left: t) (right: t) : t option =
  match (left, right) with
  | Dummy_ticket { payload = payload_a; quantity = value_a },
    Dummy_ticket { payload = payload_b; quantity = value_b } ->
      if payload_a = payload_b
      then
        let joined_ticket = Dummy_ticket {
          payload = payload_a
        ; quantity = value_a + value_b
        } in Some joined_ticket
      else None


(** [Dummy_ticket.split ticket left_part right_part]
    If [left_part + right_part] = ticket.quantity, it produces two
    new tickets. *)
let split_ticket (ticket: t) (left_amount: nat) (right_amount: nat) : splitted =
  match ticket with
  | Dummy_ticket { payload = payload; quantity = value } ->
    if value = left_amount + right_amount
    then
      let left_ticket = Dummy_ticket {
        payload = payload
      ; quantity = left_amount }
      in
      let right_ticket = Dummy_ticket {
        payload = payload
      ; quantity = right_amount }
      in
      Some (left_ticket, right_ticket)
    else None
