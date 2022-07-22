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

open Chusai_tezos

type t =
  { rpc_address : string
  ; rpc_port : int
  ; endpoint : Uri.t
  ; operator_pk : Tezos_crypto.Signature.public_key_hash
  ; inbox_address : Protocol.Contract_hash.t
  }

let make ~rpc_address ~rpc_port ~endpoint ~operator_pk ~inbox_address =
  { rpc_address; rpc_port; endpoint; operator_pk; inbox_address }
;;

let get_rpc_address { rpc_address; _ } = rpc_address
let get_rpc_port { rpc_port; _ } = rpc_port
let get_operator_pk { operator_pk; _ } = operator_pk
let get_inbox_address { inbox_address; _ } = inbox_address
let get_endpoint { endpoint; _ } = endpoint
