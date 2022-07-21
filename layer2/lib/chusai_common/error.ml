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

(* How to define errors:
   We extends the type [Tezos_base.TzPervasives.error] in order to be compatible
   with all of Tezos tooling and to be able to produce [tztrace] (and [tzresult]).

   First, we inject a new constructor:
   {[
     type t += My_error_definition of k
   ]}
   Next, we need to register it in order to be interpretable by tools
   providing:
   - an [id] (that can be used for filtering or structural logging)
   - a [title] that will be showed in [stderr]
   - a [description] that describe the errors
   - a [pp] (pretty printer) on [k] (in our example)
   - a propagation strategy (in the node we will take the habit to define everything
     as [`Permanent])
   - an [encoding] for serializing the errors that should be a JSON object that
     represents [k]. Ie: [type t += My_error of (string * int)] can be projected
     into this encoding: [Data_encoding.(obj2 (req "title" string) (req "index" int))]
   - a function from error to option
   - a function from arguments ([k]) to an error.

   Those two last parameters allow to deal generically with errors.

   /!\ Every errors from Chusai should be prefixed by [Chusai_].
*)

(* Rexport the type error as an extensible variant. *)
type t = Tezos_base.TzPervasives.error = ..

(* A dummy error for uncatched error. *)
type t += Chusai_unknown_error of string

let () =
  register_error_kind
    ~id:"chusai.unknown_error"
    ~title:"unexpected/unknown error"
    ~description:"An unknown error occurred"
    ~pp:(fun ppf -> Format.fprintf ppf {|Unknown error with message "%s"|})
    `Permanent
    Data_encoding.(obj1 (req "message" string))
    (function
     | Chusai_unknown_error message -> Some message
     | _ -> None)
    (fun message -> Chusai_unknown_error message)
;;

type t += Chusai_unable_to_parse_contract_address of string

let () =
  register_error_kind
    ~id:"chusai.unable_to_parse_contract_address"
    ~title:"unable to parse contract address"
    ~description:"The contract address is not parsable"
    ~pp:(fun ppf -> Format.fprintf ppf {|Unparable contract address "%s"|})
    `Permanent
    Data_encoding.(obj1 (req "address" string))
    (function
     | Chusai_unable_to_parse_contract_address address -> Some address
     | _ -> None)
    (fun address -> Chusai_unable_to_parse_contract_address address)
;;

type t += Chusai_invalid_script_repr of Protocol.Alpha_context.Script.expr * string

let () =
  let pp ppf (script, message) =
    Format.fprintf ppf "%s:%a" message Printer.script script
  in
  register_error_kind
    ~id:"chusai.invalid_script_repr"
    ~title:"unable to extract information from the script"
    ~description:"Invalid Script representation"
    ~pp
    `Permanent
    Data_encoding.(
      obj2
        (req "script" Protocol.Alpha_context.Script.expr_encoding)
        (req "message" string))
    (function
     | Chusai_invalid_script_repr (script, message) -> Some (script, message)
     | _ -> None)
    (fun (script, message) -> Chusai_invalid_script_repr (script, message))
;;

type t += Chusai_binary_write_error of Data_encoding.Binary.write_error

let () =
  register_error_kind
    ~id:"chusai.binary_write_error"
    ~title:"binary error"
    ~description:"An error occurs during an encoding"
    ~pp:(fun ppf -> Format.fprintf ppf "%a" Data_encoding.Binary.pp_write_error)
    `Permanent
    Data_encoding.(obj1 (req "write_error" Binary.write_error_encoding))
    (function
     | Chusai_binary_write_error err -> Some err
     | _ -> None)
    (fun err -> Chusai_binary_write_error err)
;;

type t += Chusai_node_cursor_is_higher_of_inbox_cursor of (Z.t * Z.t)

let () =
  register_error_kind
    ~id:"chusai.node_cursor_is_higher_of_inbox_cursor"
    ~title:"invalid node cursor"
    ~description:
      "the node cursor is higer than the inbox one. It is a bug, please fill an issue on \
       [https://github.com/marigold/chusai/issues]"
    ~pp:(fun ppf (node_cursor, inbox_curosr) ->
      Format.fprintf
        ppf
        "Node cursor (%a) > Inbox cursor (%a)"
        Z.pp_print
        node_cursor
        Z.pp_print
        inbox_curosr)
    `Permanent
    Data_encoding.(
      obj2 (req "node_cursor" Data_encoding.z) (req "inbox_cursor" Data_encoding.z))
    (function
     | Chusai_node_cursor_is_higher_of_inbox_cursor p -> Some p
     | _ -> None)
    (fun p -> Chusai_node_cursor_is_higher_of_inbox_cursor p)
;;

type t += Chusai_unable_to_unpack_script_repr of bytes

let () =
  register_error_kind
    ~id:"chusai.unable_to_unpack_script_repr"
    ~title:"unable to unpack script repr"
    ~description:"The unpacking (of_bytes) fail for the given bytes sequence"
    ~pp:(fun ppf bytes ->
      Format.fprintf ppf "Unable to unpack given bytes %s" @@ Bytes.to_string bytes)
    `Permanent
    Data_encoding.(obj1 (req "bytes" Data_encoding.bytes))
    (function
     | Chusai_unable_to_unpack_script_repr b -> Some b
     | _ -> None)
    (fun b -> Chusai_unable_to_unpack_script_repr b)
;;

let chusai_unknown_error_int = 1
let chusai_unable_to_parse_contract_address_int = 2
let chusai_invalid_script_repr_int = 3
let chusai_binary_write_error_int = 4
let chusai_node_cursor_is_higher_of_inbox_cursor_int = 5
let chusai_unable_to_unpack_script_repr_int = 6

let to_int = function
  | Chusai_unable_to_parse_contract_address _ ->
    chusai_unable_to_parse_contract_address_int
  | Chusai_invalid_script_repr (_, _) -> chusai_invalid_script_repr_int
  | Chusai_binary_write_error _ -> chusai_binary_write_error_int
  | Chusai_node_cursor_is_higher_of_inbox_cursor _ ->
    chusai_node_cursor_is_higher_of_inbox_cursor_int
  | Chusai_unable_to_unpack_script_repr _ -> chusai_unable_to_unpack_script_repr_int
  | Chusai_unknown_error _ | _ -> chusai_unknown_error_int
;;

let raise_ error = Error [ error ]
let raise_lwt error = Lwt.return_error [ error ]
