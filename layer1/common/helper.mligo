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

(** This module contains some utility functions that are not especially related
    to any specific module. ie: Conversion between different amount kinds. *)


(** FIXME: refer to Ligo Team

    I think it is sad to have to use arithmetic operation for natural conversion.
    I will imagine that tez and nat are aliases and just using cast (through type
    equalities) should work. ie:

    ```
    let tez_to_nat (x: tez) : nat = x
    let nat_to_tez (x: nat) : tez = x
    ``` *)

(** Converting a [tez] to a [nat]. *)
let tez_to_nat (xtz: tez) : nat =
  xtz / 1mutez (* Why? Very good question... *)

(** Converting a [nat] to a [tez]. *)
let nat_to_tez (x: nat) : tez =
  x * 1mutez