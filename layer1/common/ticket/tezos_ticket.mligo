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

(** Payload of a Ticket. *)
type payload = bytes

(** Monomorphic version a Tezos ticket parametrized with the payload. *)
type t = payload ticket

(** When a ticket has been read. *)
type been_read = (address * (payload * nat)) * t

(** When a ticket has beed splitted. *)
type splitted = (t * t) option

(** [Tezos_ticket.create_ticket payload qty]
    Create a [Ticket]. *)
let create_ticket (payload: bytes) (quantity: nat) =
  Tezos.create_ticket payload quantity

(** [Tezos_ticket.read_ticket a_dummy_ticket]
    Read a [Tezos_ticket]. *)
let read (ticket: t) : been_read =
  Tezos.read_ticket ticket

(** [Tezos_ticket.join_tickets left_ticket right_ticket]
    Join two [Tezos_ticket] of the same kind. *)
let join (left: t) (right: t) : t option =
  Tezos.join_tickets (left, right)

(** [Tezos_ticket.split ticket left_part right_part]
    If [left_part + right_part] = ticket.quantity, it produces two
    new tickets. *)
let split_ticket (ticket: t) (left_amount: nat) (right_amount: nat) : splitted =
  Tezos.split_ticket ticket (left_amount, right_amount)
