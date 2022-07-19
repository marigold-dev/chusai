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
open Tezos_error_monad.TzMonad
open Tezos_shell_services
open Chusai_tezos

let patch_state config client_context block_hash =
  (* FIXME: rebuild the state using [Big_map diff] instead of storage. *)
  let open Lwt_result_syntax in
  let chain = client_context#chain in
  let block = `Hash (block_hash, 0) in
  let chained_block = chain, block in
  let inbox_address =
    let open Protocol.Contract_hash in
    Config.get_inbox_address config |> to_b58check
  in
  let*! () = Event_log.retreiving_storage block_hash in
  let* storage =
    Chusai_bridge.Contract.get_storage client_context chained_block inbox_address
  in
  let*! () = Event_log.storage_retreived storage block_hash in
  let* () = State.patch client_context chained_block storage in
  let*! () = Event_log.state_updated block_hash in
  return ()
;;

let init_state () =
  let open Lwt_syntax in
  let* is_pending = State.is_pending () in
  if is_pending then Event_log.init_state () else return ()
;;

(* The global head handler that recompute a State. *)
let process_new_block client_context head_hash config =
  let open Lwt_result_syntax in
  (* FIXME: deal with reorganization. *)
  let*! () = Event_log.new_head head_hash in
  let*! () = init_state () in
  let* () = patch_state config client_context head_hash in
  return ()
;;

(* Monitor the head of the chain and handle reconnection using a given delay.  *)
let rec handle_head_with_reconnection ~delay config client_context =
  let open Lwt_syntax in
  let chain = client_context#chain in
  let* monitored = Monitor_services.heads client_context chain in
  match monitored with
  | Ok (stream, halt) ->
    (* If everything is ok, it returns a pair of a stream and an interruption
         function (for interrupting the streaming of the head). *)
    return_ok (stream, halt)
  | Error trace ->
    (* If the connection is lost, it try to reconnect after
         the given delay [delay].*)
    let* () =
      let s = Format.asprintf "%a" pp_print_trace trace in
      Stdlib.failwith s
    in
    let* () = Event_log.monitoring_error trace in
    let* () = Event_log.cannot_connect delay in
    let* () = Lwt_unix.sleep delay in
    handle_head_with_reconnection ~delay config client_context
;;

(* Iter over the monitored stream until an unexpected error occurs.*)
let stream_head config client_context (stream, halt) =
  let open Lwt_syntax in
  Lwt_stream.iter_s
    (fun (head_hash, _head) ->
      let* result = process_new_block client_context head_hash config in
      match result with
      | Ok () -> Lwt.return_unit
      | Error trace ->
        let* () =
          let s = Format.asprintf "%a" pp_print_trace trace in
          Stdlib.failwith s
        in
        (* Halt if an error not handled by the reconnection_handler occurs. *)
        let* () = Event_log.monitoring_error trace in
        let () = halt () in
        Lwt.return_unit)
    stream
;;

let create_client_context config =
  let open Tezos_client_base_unix in
  let rpc_config =
    { Tezos_rpc_http_client_unix.RPC_client_unix.default_config with
      endpoint = Config.get_endpoint config
    }
  in
  let ctxt =
    (* FIXME: should use user-defined parameters from the chusai-CLI or the [data-dir]
     config file. *)
    new Client_context_unix.unix_full
      ~chain:Client_config.default_chain
      ~block:Client_config.default_block
      ~confirmations:None
      ~password_filename:None
      ~base_dir:Client_config.default_base_dir
      ~rpc_config
      ~verbose_rpc_error_diagnostics:true
  in
  new Protocol_client_context.wrap_full ctxt
;;

let main_exit_callback server exit_status =
  let open Lwt_syntax in
  let* () = Rpc_server.shutdown server in
  let* () = Event_log.node_is_shutting_down exit_status in
  Tezos_base_unix.Internal_event_unix.close ()
;;

(* Start the daemon with a given reconnection [delay]. *)
let run ~delay config () =
  let open Lwt_result_syntax in
  (* Build the full protocol. *)
  let client_context = create_client_context config in
  let rpc_address = Config.get_rpc_address config in
  let rpc_port = Config.get_rpc_port config in
  let* rpc_server = Rpc_server.start config in
  let _ =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (main_exit_callback rpc_server)
  in
  let*! () = Event_log.node_is_ready ~rpc_address ~rpc_port () in
  let rec handler () =
    let* () =
      Lwt.catch
        (fun () ->
          (* Compute the stream and the interruption function. *)
          let* stream = handle_head_with_reconnection ~delay config client_context in
          (* Process the head streaming. *)
          let*! () = stream_head config client_context stream in
          let*! () = Event_log.connection_lost () in
          handler ())
        fail_with_exn
    in
    Lwt_utils.never_ending ()
  in
  handler ()
;;
