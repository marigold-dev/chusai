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
open Chusai_tezos
module Contract = Protocol.Alpha_context.Contract

type hash = Chusai_tezos.Protocol.Contract_hash.t

let get_storage rpc_context block contract_hash =
  match Contract.of_b58check contract_hash with
  | Ok contract -> Protocol.Alpha_services.Contract.storage_opt rpc_context block contract
  | Error err -> Lwt.return_error @@ Environment.wrap_tztrace err
;;

let get_big_map_value_at rpc_context block index key =
  let id = Protocol.Alpha_context.Big_map.Id.parse_z index in
  let open Lwt_result_syntax in
  let+ script = Protocol.Alpha_services.Contract.big_map_get rpc_context block id key in
  Tezos_micheline.Micheline.root script
;;
