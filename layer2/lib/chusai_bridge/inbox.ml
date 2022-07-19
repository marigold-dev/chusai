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

type message =
  | Deposit of
      { owner : string
      ; quantity : Z.t
      }

type store =
  { cursor : Z.t
  ; messages : message list Chusai_common.Map.Z.t
  }

let store cursor messages = { cursor; messages }
let empty_store = store Z.zero Chusai_common.Map.Z.empty

let message_to_script ?(location = Tezos_micheline.Micheline.dummy_location) ?(annot = [])
  =
  let loc = location in
  let open Tezos_micheline.Micheline in
  let open Chusai_tezos.Protocol.Alpha_context.Script in
  function
  | Deposit { owner; quantity } ->
    Prim (loc, D_Pair, [ String (loc + 1, owner); Int (loc + 1, quantity) ], annot)
;;

let message_from_script =
  let open Tezos_micheline.Micheline in
  let open Chusai_tezos.Protocol.Alpha_context.Script in
  function
  | Prim (_, D_Pair, [ address_node; qty_node ], _) ->
    let open Preface.Option.Monad in
    let* owner = Script.extract_string address_node in
    let+ quantity = Script.extract_natural qty_node in
    Deposit { owner; quantity }
  | _ -> None
;;

let messages_to_script_list loc messages =
  let open Chusai_common.Map.Z in
  fold
    (fun key messages acc ->
      let open Tezos_micheline.Micheline in
      let open Chusai_tezos.Protocol.Alpha_context.Script in
      let messages = List.map (message_to_script ~location:(loc + 3)) messages in
      let elt =
        Prim (loc + 1, D_Elt, [ Int (loc + 2, key); Seq (loc + 2, messages) ], [])
      in
      elt :: acc)
    messages
    []
;;

let store_to_script
    ?(location = Tezos_micheline.Micheline.dummy_location)
    ?(annot = [])
    { cursor; messages }
  =
  let loc = location in
  let open Tezos_micheline.Micheline in
  let open Chusai_tezos.Protocol.Alpha_context.Script in
  Prim
    ( loc
    , D_Pair
    , [ Int (loc + 1, cursor); Seq (loc + 1, messages_to_script_list (loc + 1) messages) ]
    , annot )
;;

let store_from_script node =
  let open Tezos_micheline.Micheline in
  let open Chusai_tezos.Protocol.Alpha_context.Script in
  match node with
  | Prim (_, D_Pair, [ Int (_, cursor); _; Int (_, bigmap_index); _; _ ], _) ->
    Some (cursor, bigmap_index)
  | _ -> None
;;

let messages_from_big_map_entry node =
  match Script.extract_seq message_from_script node with
  | Some x -> Ok x
  | None ->
    Chusai_common.Error.(
      raise_ @@ Chusai_invalid_script_repr (Script.canonicalize node, "Invalid Inbox List"))
;;

let to_ledger big_map_messages =
  let open Chusai_common in
  Map.Z.fold
    (fun _cursor messages ledger ->
      List.fold_left
        (fun ledger -> function
          | Deposit { owner; quantity } ->
            Map.String.update
              owner
              (fun value ->
                let previous_value = Option.value ~default:Z.zero value in
                Some (Z.add previous_value quantity))
              ledger)
        ledger
        messages)
    big_map_messages
    Map.String.empty
;;
