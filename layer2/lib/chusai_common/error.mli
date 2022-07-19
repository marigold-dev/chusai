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

(** Centralization of errors, as an extension of the errors defined in
    [Tezos_base.TzPervasives]. *)

open Tezos_base.TzPervasives
open Chusai_tezos

type t = Tezos_base.TzPervasives.error = ..

(** {1 Errors definition} *)

(** An error that captures unprocessed cases. *)
type t += Chusai_unknown_error of string

(** An error that captures a parsing address failure. *)
type t += Chusai_unable_to_parse_contract_address of string

(** An error that captures an invalid script representation. *)
type t += Chusai_invalid_script_repr of Protocol.Alpha_context.Script.expr * string

(** An error that captures an encoding failure. *)
type t += Chusai_binary_write_error of Data_encoding.Binary.write_error

(** An error that captures the fact that the node is not in a good state. *)
type t += Chusai_node_cursor_is_higher_of_inbox_cursor of (Z.t * Z.t)

(** An error that is raised when a bytes sequence is not a valid [Script_expr_hash.t]. *)
type t += Chusai_unable_to_unpack_script_repr of bytes

(** {1 Errors signal definition}

    Mapping from error to unix error-code. *)

val chusai_unknown_error_int : int
val chusai_unable_to_parse_contract_address_int : int
val chusai_invalid_script_repr_int : int
val chusai_binary_write_error_int : int
val chusai_node_cursor_is_higher_of_inbox_cursor_int : int
val chusai_unable_to_unpack_script_repr_int : int

(** {1 Helpers} *)

(** Map an error to an unix-exit-code. *)
val to_int : t -> int

val raise_ : t -> 'a tzresult
val raise_lwt : t -> 'a tzresult Lwt.t
