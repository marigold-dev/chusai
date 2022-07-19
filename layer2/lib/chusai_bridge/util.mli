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

(** Some common tools for bridge with layer1.*)

open Tezos_base.TzPervasives

module List_option :
  Preface.Specs.TRAVERSABLE with type 'a t = 'a option and type 'a iter = 'a list

module Seq_option :
  Preface.Specs.TRAVERSABLE with type 'a t = 'a option and type 'a iter = 'a Seq.t

module Try_trace : sig
  module Functor : Preface.Specs.FUNCTOR with type 'a t = ('a, tztrace) result
  module Applicative : Preface.Specs.APPLICATIVE with type 'a t = ('a, tztrace) result
  module Selective : Preface.Specs.SELECTIVE with type 'a t = ('a, tztrace) result
  module Monad : Preface.Specs.MONAD with type 'a t = ('a, tztrace) result
end

val hash_z : Z.t -> Chusai_tezos.Protocol.Script_expr_hash.t tzresult
val bytes_to_hex : bytes -> string
