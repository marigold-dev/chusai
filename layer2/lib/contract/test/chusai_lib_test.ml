open Chusai_contract.Contract
(*open Tezos_client_013_PtJakart*)
open Tezos_protocol_013_PtJakart.Protocol
open Tezos_protocol_013_PtJakart.Protocol.Script_ir_translator
open Tezos_protocol_013_PtJakart.Protocol.Script_typed_ir
open Tezos_error_monad.Error_monad.Legacy_monad_globals
(*open Tezos_protocol_013_PtJakart.Protocol.Michelson_v1_primitives*)
(*open Tezos_micheline.Micheline*)

let wrap_error_lwt x = x >>= fun x -> Lwt.return @@ Environment.wrap_tzresult x

let init_ctxt =
  let open Tezos_013_PtJakart_test_helpers in
  Context.init1 () >>=? fun (b, _cs) ->
  Incremental.begin_construction b >>=? fun v ->
  return @@ Incremental.alpha_ctxt v


let to_lambda () =
  let contract_expr = from_file "./contract/add.tz" in
  let storage_expr = from_string "1" in
  let script =  Alpha_context.Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr} in

  init_ctxt >>=? fun ctx ->
    wrap_error_lwt  @@
      Script_ir_translator.parse_script
      ctx
      ~legacy:false
      ~allow_forged_in_storage:false
      script
  >>=? fun (Ex_script (Script {code; storage_type; arg_type; _}) , ctxt) ->
  wrap_error_lwt @@ Lwt.return @@
    pair_t (-1) arg_type storage_type
  >>=? fun (Ty_ex_c arg_ty) ->
  wrap_error_lwt @@ Lwt.return @@
    unparse_ty ~loc:(-1) ctxt arg_ty
  >>=? fun (_arg_node, ctxt) ->
  wrap_error_lwt @@ Lwt.return @@
   (pair_t (-1) list_operation_t storage_type)
  >>=? fun (Ty_ex_c return_ty) ->
  wrap_error_lwt @@ Lwt.return @@
    unparse_ty ~loc:(-1) ctxt return_ty
  >>=? fun (_return_node, ctxt) ->
  wrap_error_lwt @@ Lwt.return @@
    lambda_t (-1) arg_ty return_ty
  >>=? fun (lambda_ty) ->
  (*wrap_error_lwt @@*)
    (*unparse_data ctxt Readable lambda_ty code*)
  (*>>=? fun (code_node, ctxt) ->*)
  wrap_error_lwt @@
  Script_ir_translator.pack_data
    ctxt
    lambda_ty
    code
  >>=? fun (bytes, _ctxt) ->

  (*let m = strip_locations (Prim ((-1), I_LAMBDA, [arg_node; return_node; code_node], [])) in*)
  let (`Hex s) = Hex.of_bytes bytes in
  Alcotest.fail s
  (*return_unit*)


let () =
  Alcotest_lwt.run "Utils" [
    "test", [ Tezos_base_test_helpers.Tztest.tztest "to lambda" `Quick to_lambda]
  ] |> Lwt_main.run
