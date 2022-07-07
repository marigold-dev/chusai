open Tezos_protocol_013_PtJakart.Protocol.Alpha_context
open Tezos_client_013_PtJakart
open Tezos_error_monad

exception Expression_from_string

(** Parse a Michelson expression from string, raising an exception on error. *)
let from_string ?(check_micheline_indentation = false) str : Script.expr =
  let ast, errs =
    Michelson_v1_parser.parse_expression ~check:check_micheline_indentation str
  in
  (match errs with
  | [] -> ()
  | lst ->
      Format.printf "expr_from_string: %a\n" Error_monad.pp_print_trace lst ;
      raise Expression_from_string) ;
  ast.expanded

let from_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ;
  from_string s
