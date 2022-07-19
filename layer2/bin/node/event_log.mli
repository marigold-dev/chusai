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

(** Structured logs propagated during the execution of the node. The logs are logged with
    the ["chusai_node"] section. *)

open Tezos_base.TzPervasives

(** Is emitted when the node is launched.*)
val node_starting : unit -> unit Lwt.t

(** Is emitted when the node is ready.*)
val node_is_ready : rpc_address:string -> rpc_port:int -> unit -> unit Lwt.t

(** Is emitted when the chusai-node cannot reach the tezos-node. *)
val cannot_connect : float -> unit Lwt.t

(** Is emitted when the connection is lost (and the daemon try to rehandle the
    connection). *)
val connection_lost : unit -> unit Lwt.t

(** Is emitted when the node is shutting down. *)
val node_is_shutting_down : int -> unit Lwt.t

(** Is emitted when a new head is monitored. *)
val new_head : Block_hash.t -> unit Lwt.t

(** Is emitted when the state is computed. *)
val init_state : unit -> unit Lwt.t

(** Is emitted when an error occurs during the head monitoring. *)
val monitoring_error : tztrace -> unit Lwt.t

(** Is emitted when the node will fetch the Inbox storage. *)
val retreiving_storage : Block_hash.t -> unit Lwt.t

(** Is emitted when a storage was retreiving from the Inbox contract. *)
val storage_retreived
  :  Chusai_tezos.Protocol.Alpha_context.Script.expr option
  -> Block_hash.t
  -> unit Lwt.t

(** Is emitted when the state was updated. *)
val state_updated : Block_hash.t -> unit Lwt.t

(** Is emitted when a computed store is empty. *)
val store_is_empty : unit -> unit Lwt.t

(** Is emitted when an inbox is recomputed at a particular level. *)
val recomputing_inbox_for_cursor : Z.t -> unit Lwt.t

(** Is emitted when the RPC server is starting. *)
val rpc_server_starting : unit -> unit Lwt.t

(** Is emitted when the RPC server is shutting down. *)
val rpc_server_is_shutting_down : unit -> unit Lwt.t
