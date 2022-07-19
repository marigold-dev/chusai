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

open Chusai_tezos

let port =
  let docv = "A port, mostly used for RPC address" in
  let printer ppf value = Format.fprintf ppf "%d" value in
  let port_parser value =
    match int_of_string_opt value with
    | None ->
      let msg = `Msg "The given string is not an integer" in
      Result.error msg
    | Some value ->
      if value < 0
      then (
        let msg = `Msg "Port is lower than [0]" in
        Result.error msg)
      else if value > 65535
      then (
        let msg = `Msg "Port is greater than [65535]" in
        Result.error msg)
      else Result.ok value
  in
  Cmdliner.Arg.conv ~docv (port_parser, printer)
;;

let signature =
  let docv = "A signature" in
  let printer = Tezos_crypto.Signature.pp in
  let sig_parser value =
    match Tezos_crypto.Signature.of_b58check_opt value with
    | None ->
      let msg = `Msg "Invalid Signature" in
      Result.error msg
    | Some signature -> Result.ok signature
  in
  Cmdliner.Arg.conv ~docv (sig_parser, printer)
;;

let public_key_hash =
  let docv = "A signature's public key hash" in
  let printer = Tezos_crypto.Signature.Public_key_hash.pp in
  let sig_parser value =
    match Tezos_crypto.Signature.Public_key_hash.of_b58check_opt value with
    | None ->
      let msg = `Msg "Invalid Public key hash" in
      Result.error msg
    | Some public_key_hash -> Result.ok public_key_hash
  in
  Cmdliner.Arg.conv ~docv (sig_parser, printer)
;;

let contract_hash =
  let docv = "A contract hash (prefixed by KT1)" in
  let printer = Protocol.Contract_hash.pp in
  let contract_parser value =
    match Protocol.Contract_hash.of_b58check_opt value with
    | None ->
      let msg = `Msg "Invalid contract hash" in
      Result.error msg
    | Some contract_hash -> Result.ok contract_hash
  in
  Cmdliner.Arg.conv ~docv (contract_parser, printer)
;;

let p2p_point =
  let docv = "A pair of address/port in the form [address:port]" in
  let printer = Tezos_base.P2p_point.Id.pp in
  let p2p_parser value =
    match Tezos_base.P2p_point.Id.of_string value with
    | Ok x -> Ok x
    | Error msg -> Result.error (`Msg msg)
  in
  Cmdliner.Arg.conv ~docv (p2p_parser, printer)
;;

let uri =
  let docv = "An URI.t (characterizing an URI)" in
  let printer = Uri.pp in
  let uri_parser value = value |> Uri.of_string |> Result.ok in
  Cmdliner.Arg.conv ~docv (uri_parser, printer)
;;
