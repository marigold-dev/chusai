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
open Tezos_rpc
open Tezos_rpc_http
open Tezos_rpc_http_server
open Chusai_common

let register_balances dir =
  RPC_directory.register0 dir (Chusai_rpc.balances ()) (fun () () ->
      let open Lwt_result_syntax in
      let*! _node_cursor, node_messages = State.extract_cursor_messages () in
      let ledger = Chusai_bridge.Inbox.to_ledger node_messages |> Map.String.bindings in
      return ledger)
;;

let register_balance_for dir =
  RPC_directory.register1 dir (Chusai_rpc.balance_for ()) (fun address () () ->
      let open Lwt_result_syntax in
      let*! _node_cursor, node_messages = State.extract_cursor_messages () in
      let ledger = Chusai_bridge.Inbox.to_ledger node_messages in
      return @@ Map.String.find_opt address ledger)
;;

let register () = RPC_directory.empty |> register_balances |> register_balance_for

let launch ~host ~acl ~node ~dir () =
  let open Lwt_result_syntax in
  let*! server =
    RPC_server.launch ~media_types:Media_type.all_media_types ~host ~acl node dir
  in
  let*! () = Event_log.rpc_server_starting () in
  return server
;;

let start configuration =
  let rpc_addr = Config.get_rpc_address configuration in
  let rpc_port = Config.get_rpc_port configuration in
  let addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string addr in
  let dir = register () in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.default addr in
  Lwt.catch (launch ~host ~acl ~node ~dir) fail_with_exn
;;

let shutdown server =
  let open Lwt_syntax in
  let* () = Event_log.rpc_server_is_shutting_down () in
  RPC_server.shutdown server
;;
