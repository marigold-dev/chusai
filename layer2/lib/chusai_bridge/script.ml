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

type t = Protocol.Alpha_context.Script.node

let canonicalize = Tezos_micheline.Micheline.strip_locations
let root = Tezos_micheline.Micheline.root

let extract_natural =
  let open Tezos_micheline.Micheline in
  function
  | Int (_, value) -> Some value
  | _ -> None
;;

let extract_string =
  let open Tezos_micheline.Micheline in
  function
  | String (_, value) -> Some value
  | _ -> None
;;

let extract_pair =
  let open Tezos_micheline.Micheline in
  let open Protocol.Alpha_context.Script in
  function
  | Prim (_, D_Pair, [ left; right ], _) -> Some (left, right)
  | _ -> None
;;

let extract_seq f =
  let open Tezos_micheline.Micheline in
  function
  | Seq (_, node_list) -> Util.List_option.traverse f node_list
  | _ -> None
;;

let extract_big_map check_key check_value =
  let open Tezos_micheline.Micheline in
  let open Protocol.Alpha_context.Script in
  extract_seq (function
    | Prim (_, D_Elt, [ key; value ], _) ->
      let open Preface.Option.Monad in
      let* key = check_key key in
      let* value = check_value value in
      return (key, value)
    | _ -> None)
;;

let encode node =
  let script = canonicalize node in
  let encoding = Protocol.Alpha_context.Script.expr_encoding in
  let open Util.Try_trace.Monad in
  let+ bytes =
    Data_encoding.Binary.to_bytes encoding script
    |> Result.map_error (fun x -> [ Chusai_common.Error.Chusai_binary_write_error x ])
  in
  Bytes.(cat (of_string "\005") bytes)
;;

let to_hex node =
  let open Util.Try_trace.Monad in
  let+ bytes = encode node in
  Util.bytes_to_hex bytes
;;

let z x =
  let open Tezos_micheline.Micheline in
  Int (dummy_location, x)
;;

let int x = z @@ Z.of_int x

let elt n =
  let open Tezos_micheline.Micheline in
  let open Protocol.Alpha_context.Script in
  Prim (dummy_location, D_Elt, n, [])
;;

let to_script_expr_hash node =
  let open Util.Try_trace.Monad in
  let* bytes = encode node in
  try return @@ Protocol.Script_expr_hash.hash_bytes [ bytes ] with
  | _ ->
    Chusai_common.Error.(
      raise_
      @@ Chusai_invalid_script_repr (canonicalize node, "[Invalid Script_expr_hash]"))
;;
