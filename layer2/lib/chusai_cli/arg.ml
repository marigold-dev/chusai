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

let docs = Cmdliner.Manpage.s_common_options

let rpc_address ?(default_value = "127.0.0.1") () =
  let open Cmdliner in
  let doc =
    Format.asprintf
      "The address where the node listens to RPCs (default value: %s)"
      default_value
  in
  let arg = Arg.info ~doc ~docs [ "rpc-address" ] in
  Arg.(value & opt string default_value arg)
;;

let rpc_port ?(default_value = 9999) () =
  let open Cmdliner in
  let doc =
    Format.asprintf
      "The port where the node listens to RPCs (default value: %04d)"
      default_value
  in
  let arg = Arg.info ~doc ~docs [ "rpc-port" ] in
  Arg.(value & opt Conv.port default_value arg)
;;

let endpoint =
  let open Cmdliner in
  let doc = "scheme://address:port of the tezos-node" in
  let arg = Arg.info ~doc ~docs [ "endpoint"; "node-endpoint" ] in
  Arg.(required & opt (some Conv.uri) None & arg)
;;

let operator =
  let open Cmdliner in
  let doc = "Public key hash (Base58) of the rollup operator" in
  let arg = Arg.info ~doc ~docs [ "operator"; "rollup-operator" ] in
  Arg.(required & opt (some Conv.public_key_hash) None & arg)
;;

let wallet_address =
  let open Cmdliner in
  let doc = "Wallet Address" in
  let arg = Arg.info ~doc ~docs [ "wallet"; "wallet-sc" ] in
  Arg.(required & opt (some Conv.contract_hash) None & arg)
;;

let rollup_address =
  let open Cmdliner in
  let doc = "Rollup Address" in
  let arg = Arg.info ~doc ~docs [ "rollup"; "rollup-sc"; "inbox"; "inbox-sc" ] in
  Arg.(required & opt (some Conv.contract_hash) None & arg)
;;

let inbox_address = rollup_address
