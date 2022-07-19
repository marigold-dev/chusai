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

open Tezos_base.TzPervasives

(** Some tools to manipulate Micheline expressions. *)

type t = Chusai_tezos.Protocol.Alpha_context.Script.node

(** {1 Extract values} *)

val extract_natural : t -> Z.t option
val extract_string : t -> string option

val extract_big_map
  :  (t -> 'key option)
  -> (t -> 'value option)
  -> t
  -> ('key * 'value) list option

val extract_pair : t -> (t * t) option
val extract_seq : (t -> 'value option) -> t -> 'value list option

(** {1 Expr creation} *)

val z : Z.t -> t
val int : int -> t
val elt : t list -> t

(** {1 Util} *)

val encode : t -> bytes tzresult
val to_hex : t -> string tzresult
val hash : t -> Chusai_tezos.Protocol.Script_expr_hash.t tzresult
val canonicalize : t -> Chusai_tezos.Protocol.Alpha_context.Script.expr
val root : Chusai_tezos.Protocol.Alpha_context.Script.expr -> t
