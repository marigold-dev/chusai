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

(** Converter for [Cmdliner.Arg]. *)

open Chusai_tezos

(** A [port] is encoded on 16bits so it is an integer that have to be include into the
    range [\[0..2^16\]]. The validation scheme just checks the bound of the value. *)
val port : int Cmdliner.Arg.conv

(** A converter for operator signature. *)
val signature : Tezos_crypto.Signature.t Cmdliner.Arg.conv

(** A converter for operator public key hash. *)
val public_key_hash : Tezos_crypto.Signature.public_key_hash Cmdliner.Arg.conv

(** A converter for a contract hash. *)
val contract_hash : Protocol.Contract_hash.t Cmdliner.Arg.conv

(** A converter for p2p_point (address x port). *)
val p2p_point : Tezos_base.P2p_point.Id.t Cmdliner.Arg.conv

(** A converter for [Uri.t]. *)
val uri : Uri.t Cmdliner.Arg.conv
