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
open Chusai_common
open Chusai_bridge

type t =
  | Pending
  | Processing of Inbox.store

let current_state = ref Pending

let extract_cursor_messages () =
  match !current_state with
  | Pending ->
    let Inbox.{ cursor; messages } = Inbox.empty_store in
    Lwt.return (cursor, messages)
  | Processing { cursor; messages } -> Lwt.return (cursor, messages)
;;

let process_storage_until rpc_context block node_messages node_cursor inbox_cursor index =
  let rec aux node_messages node_cursor =
    let open Lwt_result_syntax in
    let*! () = Event_log.recomputing_inbox_for_cursor node_cursor in
    (* Recompute inboxes at each level *)
    let* script_cursor = Lwt.return @@ Script.(hash @@ z node_cursor) in
    let* big_map_result =
      Contract.get_big_map_value_at rpc_context block index script_cursor
    in
    let* messages_at = Lwt.return @@ Inbox.messages_from_big_map_entry big_map_result in
    let new_messages = Map.Z.add node_cursor messages_at node_messages in
    if Z.equal node_cursor inbox_cursor
    then (
      let () = current_state := Processing (Inbox.store node_cursor new_messages) in
      return ())
    else aux new_messages (Z.succ node_cursor)
  in
  aux node_messages node_cursor
;;

let recompute rpc_context block inbox_cursor message_big_map_index =
  let open Lwt.Syntax in
  let* node_cursor, node_messages = extract_cursor_messages () in
  if inbox_cursor < node_cursor
  then
    Error.(
      raise_lwt @@ Chusai_node_cursor_is_higher_of_inbox_cursor (node_cursor, inbox_cursor))
  else
    process_storage_until
      rpc_context
      block
      node_messages
      node_cursor
      inbox_cursor
      message_big_map_index
;;

let patch rpc_context block potential_storage =
  let open Lwt_result_syntax in
  match potential_storage with
  | Some storage ->
    let script = Script.root storage in
    (match Inbox.store_from_script script with
    | Some (inbox_cursor, message_big_map_index) ->
      recompute rpc_context block inbox_cursor message_big_map_index
    | None ->
      Error.(raise_lwt @@ Chusai_invalid_script_repr (storage, "Invalid Inbox Storage")))
  | None ->
    let*! () = Event_log.store_is_empty () in
    return ()
;;

let is_pending () =
  Lwt.return
    (match !current_state with
    | Pending -> true
    | Processing _ -> false)
;;
