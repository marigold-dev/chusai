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

module Simple = struct
  include Tezos_event_logging.Internal_event.Simple

  let section = [ "chusai_node" ]

  let node_starting =
    declare_0
      ~section
      ~name:"chusai_node_starting"
      ~msg:"starting the chusai-node"
      ~level:Notice
      ()
  ;;

  let rpc_server_starting =
    declare_0
      ~section
      ~name:"chusai_node_rpc_server_starting"
      ~msg:"starting the RPC-server of the chusai-node"
      ~level:Notice
      ()
  ;;

  let node_is_ready =
    declare_2
      ~section
      ~name:"chusai_node_is_ready"
      ~msg:"starting the chusai-node on {rpc-address}:{rpc-port}"
      ~level:Notice
      ("rpc-address", Data_encoding.string)
      ("rpc-port", Data_encoding.uint16)
  ;;

  let cannot_connect =
    declare_1
      ~section
      ~name:"chusai_node_cannot_connect_to_tezos_node"
      ~msg:"the chusai-node cannot connect to the tezos node, retrying in {delay}s"
      ~level:Warning
      ("delay", Data_encoding.float)
  ;;

  let connection_lost =
    declare_0
      ~section
      ~name:"chusai_node_connection_lost"
      ~msg:"connection to the chusai-node has been lost"
      ~level:Warning
      ()
  ;;

  let new_head =
    declare_1
      ~section
      ~name:"chusai_node_new_head"
      ~msg:"new head with hash: {head_hash}"
      ~level:Notice
      ("head_hash", Block_hash.encoding)
  ;;

  let node_is_shutting_down =
    declare_1
      ~section
      ~name:"chusai_node_shutting_down"
      ~msg:"the chusai-node is shutting down with code {exit_code}"
      ~level:Notice
      ("exit_code", Data_encoding.int31)
  ;;

  let rpc_server_is_shutting_down =
    declare_0
      ~section
      ~name:"chusai_node_rpc_server_shutting_down"
      ~msg:"the RPC server of the chusai-node is shutting down"
      ~level:Notice
      ()
  ;;

  let init_state =
    declare_0
      ~section
      ~name:"chusai_node_init_state"
      ~msg:"initialize the state using the contract storage"
      ~level:Notice
      ()
  ;;

  let monitoring_error =
    declare_1
      ~section
      ~name:"chusai_node_monitoring_error"
      ~msg:"Error during the monitoring: \n{message}"
      ~level:Error
      ("message", Data_encoding.string)
  ;;

  let retreiving_storage =
    declare_1
      ~section
      ~name:"chusai_node_retreiving_storage"
      ~msg:"Retreiving storage of Inbox SC for block {block_hash}"
      ~level:Notice
      ("block_hash", Block_hash.encoding)
  ;;

  let storage_retreived =
    declare_2
      ~section
      ~name:"chusai_node_storage_retreived"
      ~msg:"Store fetched from Inbox SC for block {block_hash}, {script}"
      ~level:Notice
      ( "script"
      , Data_encoding.option Chusai_tezos.Protocol.Alpha_context.Script.expr_encoding )
      ("block_hash", Block_hash.encoding)
  ;;

  let state_updated =
    declare_1
      ~section
      ~name:"chusai_node_state_uodated"
      ~msg:"State update for block {block_hash}"
      ~level:Notice
      ("block_hash", Block_hash.encoding)
  ;;

  let store_is_empty =
    declare_0
      ~section
      ~name:"chusai_node_store_is_empty"
      ~msg:"the storage is empty"
      ~level:Notice
      ()
  ;;

  let recomputing_inbox_for_cursor =
    declare_1
      ~section
      ~name:"chusai_node_recomputing_inbox"
      ~msg:"Recomputing inbox for cursor {cursor}"
      ~level:Notice
      ("cursor", Data_encoding.z)
  ;;
end

let node_starting = Simple.(emit node_starting)

let node_is_ready ~rpc_address ~rpc_port () =
  Simple.(emit node_is_ready (rpc_address, rpc_port))
;;

let cannot_connect = Simple.(emit cannot_connect)
let connection_lost = Simple.(emit connection_lost)
let new_head = Simple.(emit new_head)
let node_is_shutting_down = Simple.(emit node_is_shutting_down)
let init_state = Simple.(emit init_state)

let monitoring_error errors =
  let error = Format.asprintf "%a" pp_print_top_error_of_trace errors in
  Simple.(emit monitoring_error) error
;;

let retreiving_storage = Simple.(emit retreiving_storage)

let storage_retreived script_opt block =
  Simple.(emit storage_retreived) (script_opt, block)
;;

let state_updated = Simple.(emit state_updated)
let store_is_empty = Simple.(emit store_is_empty)

let recomputing_inbox_for_cursor cursor =
  Simple.(emit recomputing_inbox_for_cursor) cursor
;;

let rpc_server_starting = Simple.(emit rpc_server_starting)
let rpc_server_is_shutting_down = Simple.(emit rpc_server_is_shutting_down)
