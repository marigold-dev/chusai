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

(** Entry point for [Mint_sc]. *)

#import "ticket/chusai_ticket.mligo" "Ticket"

(** Since we cannot match using a Longident Path, endpoints constructors are
    prefixed by the smart-contract "owner" of the endpoint. *)
type t =
  | Mint_mint of Ticket.t contract (* Receive callback from [Wallet_sc] and mint a ticket
                                      and send it through the callback. So the
                                      [Ticket.t contract] is the caller of the [Mint]
                                      entrypoint. *)

(** A target is an image of [t], for dealing with with string representation.
    Every entrypoint should have an image into [target]. *)
type target =
  | Mint_mint_target

(** Gives a canonical reprsentation of an endpoint using a target. *)
let to_string (target : target) : string =
  match target with
  | Mint_mint_target -> "%mint_mint"
