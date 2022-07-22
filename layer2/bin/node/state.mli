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

(** An [in-memory] representation of the state of the [chusai-node]. It is used to
    maintain a consistent state throughout the life cycle of the monitored rollup. *)

open Tezos_base.TzPervasives
open Chusai_tezos

(* FIXME: should be replaced by the [Merkle_map]. *)

type t

(** {1 Helpers} *)

(** [patch contract_storage] patch the state in a processing mode. *)
val patch
  :  'a #Environment.RPC_context.simple
  -> 'a
  -> Protocol.Alpha_context.Script.expr option
  -> unit tzresult Lwt.t

(** [is_pending state] returns [true] if the state was not already initialized, otherwise
    [false]. *)
val is_pending : unit -> bool Lwt.t

(** Extract a pair of [cursor] and [messages map] of the current storage. *)
val extract_cursor_messages
  :  unit
  -> (Z.t * Chusai_bridge.Inbox.message list Chusai_common.Map.Z.t) Lwt.t
