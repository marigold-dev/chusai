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

(** This file is present to facilitate the switch between [Dummy_ticket.t] and
    regular Tezos tickets.

    [Dummy_ticket] contains a "dummy implementation of ticket" and [Tezos_ticket]
    contains the real one. So you have to switch from [dummy_ticket] to [tezos_ticket]
    for dealing with real ticket. *)

#include "dummy_ticket.mligo"
(* #include "tezos_ticket.mligo" *)

(** Try to join a ticket with a potential non-existing ticket. *)
let may_join_tickets (left: t option) (right: t) : t option =
  match left with
  | None -> Some right
  | Some left_ticket -> join_tickets left_ticket right
