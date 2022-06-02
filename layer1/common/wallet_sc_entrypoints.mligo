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

(** Entry point for [Wallet_sc]. *)

#import "ticket/chusai_ticket.mligo" "Ticket"

(** Since we cannot match using a Longident Path, endpoints constructors are
    prefixed by the smart-contract "owner" of the endpoint. *)
type t =
  | Wallet_request_mint of address      (** Request a [mint] to a [mint] smart_contract. *)
  | Wallet_retreive_ticket of Ticket.t  (** Retreive a ticket just after minting.  *)
  | Wallet_deposit of address           (** Make a deposit to a rollup. *)

(** A target is an image of [t], for dealing with with string representation.
    Every entrypoint should have an image into [target]. *)
type target =
  | Wallet_request_mint_target
  | Wallet_retreive_ticket_target
  | Wallet_deposit_target

(** Gives a canonical reprsentation of an endpoint using a target.  *)
let to_string (target : target) : string =
  match target with
  | Wallet_request_mint_target -> "%wallet_request_mint"
  | Wallet_retreive_ticket_target -> "%wallet_retreive_ticket"
  | Wallet_deposit_target -> "%wallet_deposit"
