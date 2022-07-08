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

(* Segment library *)
(* Defines the API on the basic structures of the game :
    - a segment
    - a split  
*)
 
#include "../../commons/refutation_interface.mligo"

let make_segment (start:hash) (ending:hash) (size:size) =
    (start,ending,size)

let size (s : segment ) =
    s.2

let start (s : segment ) =
    s.0

let ending (s : segment ) =
    s.1

(** chooses in a [split] the segment corresponding to a given [choice] *)
let choose (choice, split : choice * split) : segment = 
    match choice with
    | Left  -> split.0
    | Right -> split.1

(** checks that a [split] is well-formed *)
let split_is_consistent ((s1,s2) : split) : bool =
    (ending s1) = (start s2)

(** checks that a [split] is a correct attack of a [segment]:
    it is an _attack_ : so the starting point is the same but the end must be different
*)
let check_split_against_segment ((s1,s2),segment : split * segment) : bool = 
       (start segment) = (start s1)
    && split_is_consistent (s1,s2)
    && (size segment) = ((size s1) + (size s2))
    && (ending segment) <> (ending s2)    