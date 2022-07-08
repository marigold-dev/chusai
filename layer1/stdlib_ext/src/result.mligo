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
    
(* Definition of Result module: handle computation results and errors in an explicit and declarative manner without resorting to exceptions*)
 
(** the type of result value, which is either [Ok v] or [Error e] *)
type ('a, 'b) t = Ok of 'a | Error of 'b

(** is the result an error *)
let is_error (type a b) (result : (a, b) t) : bool = match result with
    | Error _ -> true
    | _ -> false

(** is the result ok*)
let is_ok (type a b) (result : (a, b) t) : bool = match result with
    | Error _ -> false
    | _ -> true

(** [get_ok_or_raises(Ok v)] is [v] or raises with [msg] *)
let get_ok_or_raises (type a b) (result : (a, b) t) (pp: b -> string) : a = match result with
    | Error e -> failwith (pp e)
    | Ok g -> g
