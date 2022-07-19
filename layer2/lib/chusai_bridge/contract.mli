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

open Tezos_error_monad.TzMonad
open Chusai_tezos

(** {1 Types} *)

type hash = Chusai_tezos.Protocol.Contract_hash.t

(** {1 Helpers} *)

val get_storage
  :  'a #Environment.RPC_context.simple
  -> 'a
  -> string
  -> Chusai_tezos.Protocol.Alpha_context.Script.expr option tzresult Lwt.t

val get_big_map_value_at
  :  'a #Environment.RPC_context.simple
  -> 'a
  -> Z.t
  -> Protocol.Script_expr_hash.t
  -> Script.t tzresult Lwt.t
