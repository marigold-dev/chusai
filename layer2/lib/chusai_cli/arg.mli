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

(** describes the set of CLI arguments that can be used in the various Chusai binaries. *)

open Chusai_tezos

(** Describes the [--rpc-address] arg. *)
val rpc_address : ?default_value:string -> unit -> string Cmdliner.Term.t

(** Describes the [--rpc-port] arg. *)
val rpc_port : ?default_value:int -> unit -> int Cmdliner.Term.t

(** Describes the [--endpoint] arg. *)
val endpoint : Uri.t Cmdliner.Term.t

(** Describes the [--operator] arg. *)
val operator : Tezos_crypto.Signature.public_key_hash Cmdliner.Term.t

(** Describes the [--wallet], an adress that refer the [Wallet_sc] used to interact with
    [Mint_sc]. *)
val wallet_address : Protocol.Contract_hash.t Cmdliner.Term.t

(** Describes the [--rollup], an address that refer the rollup semantic. *)
val rollup_address : Protocol.Contract_hash.t Cmdliner.Term.t

(** Describes the [--inbox], {i atm}, the inbox smart-contract is the same of
    [rollup_addres]. *)
val inbox_address : Protocol.Contract_hash.t Cmdliner.Term.t
