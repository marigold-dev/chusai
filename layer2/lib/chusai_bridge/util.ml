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
module List_option = Preface.List.Monad.Traversable (Preface.Option.Monad)
module Seq_option = Preface.Seq.Monad.Traversable (Preface.Option.Monad)

module Try_trace = struct
  module Trace = struct
    type t = tztrace
  end

  module Functor = Preface.Result.Functor (Trace)
  module Applicative = Preface.Result.Applicative (Trace)
  module Selective = Preface.Result.Selective (Trace)
  module Monad = Preface.Result.Monad (Trace)
end

let hash_z z =
  let encoding = Data_encoding.z in
  let open Try_trace.Monad in
  let* bytes =
    Data_encoding.Binary.to_bytes encoding z
    |> Result.map_error (fun x -> [ Chusai_common.Error.Chusai_binary_write_error x ])
  in
  match Chusai_tezos.Protocol.Script_expr_hash.of_bytes_opt bytes with
  | Some hash -> return hash
  | None -> Chusai_common.Error.(Error [ Chusai_unable_to_unpack_script_repr bytes ])
;;

let bytes_to_hex bytes =
  let str = Bytes.to_string bytes in
  Fmt.(str "%a" @@ on_string (octets ~sep:nop ())) str
;;
