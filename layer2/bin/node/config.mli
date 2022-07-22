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

(** Describes the configuration of the node, an immutable structure that maintains the
    parameters supplied as arguments to the node. *)

open Chusai_tezos

(** The configuration is a record with hidden fields. *)
type t

(** Initialize a new configuration state.*)
val make
  :  rpc_address:string
  -> rpc_port:int
  -> endpoint:Uri.t
  -> operator_pk:Tezos_crypto.Signature.public_key_hash
  -> inbox_address:Protocol.Contract_hash.t
  -> t

(** Retreive the [rpc_address]. *)
val get_rpc_address : t -> string

(** Retreive the [rpc_port]. *)
val get_rpc_port : t -> int

(** Retreive the [operator signature]. *)
val get_operator_pk : t -> Tezos_crypto.Signature.public_key_hash

(** Retreive the [address] of the inbox smart-contract.. *)
val get_inbox_address : t -> Protocol.Contract_hash.t

(** Retreive the [tezos-node] endpoint. *)
val get_endpoint : t -> Uri.t
