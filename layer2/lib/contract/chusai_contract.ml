open Tezos_protocol_013_PtJakart.Protocol
open Tezos_error_monad.Error_monad.Legacy_monad_globals

let from_string = Tezos_013_PtJakart_test_helpers.Expr.from_string

let from_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ;
  from_string s
  
(** [init_tezos_ctxt] initializes dummy tezos context *)
let init_tezos_ctxt =
  let open Tezos_013_PtJakart_test_helpers in
  Context.init1 () >>=? fun (b, _cs) ->
  Incremental.begin_construction b >>=? fun v ->
  return @@ Incremental.alpha_ctxt v

let pack_chusai_contract chusai_contract_expr =
  let open Tezos_013_PtJakart_test_helpers.Error_monad_operators in
  let chusai_storage_expr = from_string "None" in
  let script = Alpha_context.Script.{code = lazy_expr chusai_contract_expr; storage = lazy_expr chusai_storage_expr} in
  init_tezos_ctxt >>=? fun ctx ->
  Script_ir_translator.parse_script
    ctx
    ~legacy:false
    ~allow_forged_in_storage:false
    script
  >>=?? fun (Ex_script (Script {code; storage_type; arg_type; _}) , ctxt) ->
    Script_typed_ir.pair_t (-1) arg_type storage_type
  >>??= fun (Ty_ex_c arg_ty) ->
   (Script_typed_ir.pair_t (-1) Script_typed_ir.list_operation_t storage_type)
  >>??= fun (Ty_ex_c return_ty) ->
    Script_typed_ir.lambda_t (-1) arg_ty return_ty
  >>??= fun (lambda_ty) ->
  Script_ir_translator.pack_data
    ctxt
    lambda_ty
    code
  >>=?? fun (bytes, _ctxt) ->
  return bytes

let lift_execution_arg ctxt entrypoint_ty
    construct (arg : Alpha_context.Script.expr)  =
  let arg = Tezos_micheline.Micheline.root arg in
  Script_ir_translator.parse_data ctxt ~legacy:false ~allow_forged:false entrypoint_ty arg
  >>=? fun (entrypoint_arg, ctxt) -> return (construct entrypoint_arg, ctxt)

let execute ~script_expr ~storage_expr ~arg_expr ~entrypoint =
  let open Tezos_013_PtJakart_test_helpers.Error_monad_operators in
  let script = Alpha_context.Script.{code = lazy_expr script_expr; storage = lazy_expr storage_expr} in
  init_tezos_ctxt >>=? fun ctxt ->
  Script_ir_translator.parse_script
    ctxt
    ~legacy:false
    ~allow_forged_in_storage:false
    script
  >>=?? fun (Ex_script (Script {code; storage_type; arg_type; storage; entrypoints; _}) , ctxt) ->
  Gas_monad.run
    ctxt
    (Script_ir_translator.find_entrypoint ~error_details:Informative arg_type entrypoints entrypoint)
  >>??= fun (r, ctxt) -> r
  >>??= fun (Script_ir_translator.Ex_ty_cstr {ty = entrypoint_ty; construct; original_type_expr = _}) ->
  lift_execution_arg ctxt entrypoint_ty construct arg_expr
  >>=?? fun (arg, ctxt) ->
  Chusai_script_interpreter.interp None (ctxt, Tezos_013_PtJakart_test_helpers.Contract_helpers.default_step_constants) code (arg, storage)
  >>=? fun ((_ops, new_storage), ctxt) ->
    Chusai_script_interpreter.wrap_tztrace_lwt @@
    Script_ir_translator.pack_data
    ctxt
    storage_type
    new_storage
  >>=? fun (bytes, _ctxt) ->
    return bytes
