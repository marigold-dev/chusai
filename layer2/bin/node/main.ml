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

open Chusai_cli
module Chusai_arg = Arg
open Tezos_error_monad.TzMonad

let caller = Sys.argv.(0)
let version = "dev"

let run rpc_address rpc_port endpoint operator_pk inbox_address =
  let open Lwt_result_syntax in
  let config = Config.make ~rpc_address ~rpc_port ~endpoint ~operator_pk ~inbox_address in
  let promise =
    let*! () = Tezos_base_unix.Internal_event_unix.init () in
    let*! () = Event_log.node_starting () in
    Daemon.run ~delay:2.0 config ()
  in
  Lwt_main.run
    (let*! result = Lwt_exit.wrap_and_exit promise in
     match result with
     | Ok () ->
       let*! _ = Lwt_exit.exit_and_wait 0 in
       Lwt.return (`Ok ())
     | Error trace ->
       let*! _ = Lwt_exit.exit_and_wait 1 in
       Lwt.return @@ `Error (false, Format.asprintf "%a" pp_print_trace trace))
;;

let action_run =
  let open Cmdliner in
  let doc = "Run the [chusai-node] along with the [tezos-node]" in
  let exits = Cmd.Exit.defaults in
  let info = Cmd.info "run" ~doc ~exits in
  let term =
    Term.(
      ret
        (const run
        $ Chusai_arg.rpc_address ~default_value:"localhost" ()
        $ Chusai_arg.rpc_port ~default_value:9999 ()
        $ Chusai_arg.endpoint
        $ Chusai_arg.operator
        $ Chusai_arg.inbox_address))
  in
  Cmd.v info term
;;

let actions = [ action_run ]

let main =
  let open Cmdliner in
  let doc = "Chusai node" in
  let sdocs = Manpage.s_common_options in
  let exits = Termination.all in
  let info = Cmd.info caller ~version ~doc ~sdocs ~exits in
  let default_action = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default:default_action actions
;;

let () = exit @@ Cmdliner.Cmd.eval main
