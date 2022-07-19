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

(** Deal with Layer1's inboxes *)

type message =
  | Deposit of
      { owner : string
      ; quantity : Z.t
      }

type store =
  { cursor : Z.t
  ; messages : message list Chusai_common.Map.Z.t
  }

val store : Z.t -> message list Chusai_common.Map.Z.t -> store
val empty_store : store
val message_to_script : ?location:int -> ?annot:string list -> message -> Script.t
val message_from_script : Script.t -> message option
val store_to_script : ?location:int -> ?annot:string list -> store -> Script.t
val store_from_script : Script.t -> (Z.t * Z.t) option
val messages_from_big_map_entry : Script.t -> message list tzresult
val to_ledger : message list Chusai_common.Map.Z.t -> Z.t Chusai_common.Map.String.t
