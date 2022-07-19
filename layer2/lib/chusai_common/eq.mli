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

(** As the current protocol is abstracted by the [chusai_tezos] package, it sometimes
    happens that some type equality is complex to restore (especially because some types
    are heavily re-exported, like errors).

    [Refl] allows the explicit manipulation of type equalities.*)

type (_, _) t = Refl : ('a, 'a) t

(** {1 Refl manipulation} *)

(** [symetry Refl] construct a proof that type equality is symmetric *)
val symmetry : ('a, 'b) t -> ('b, 'a) t

(** [transitivity Refl Refl] construct a proof that type equalities are transitives. *)
val transitivity : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

(** {2 Dealing with injectivity}

    The absence of Higher Kinded Types at the level of functions implies that a functor
    must be used to describe a surjective equality relation. *)

module For (T : sig
  type 'a t
end) : sig
  val up : ('a, 'b) t -> ('a T.t, 'b T.t) t
end

(** {1 Helpers} *)

(** [cast Refl x] safely cast [x] from ['a] to ['b]. *)
val cast : ('a, 'b) t -> 'a -> 'b
