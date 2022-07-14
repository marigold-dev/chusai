open Tezos_protocol_013_PtJakart.Protocol
open Tezos_error_monad.Error_monad
open Tezos_error_monad.Error_monad.Legacy_monad_globals

module S = Saturation_repr

let wrap_tztrace_lwt x =
  x >>= function
  | Error e -> Lwt.return @@ Error (Environment.wrap_tztrace e)
  | Ok ok -> return ok

let wrap_tzerror_lwt x =
  x >>= function
 | Ok ok -> return ok
 | Error es -> Lwt.return @@ Error (List.map Environment.wrap_tzerror es)

type error += Reject of Alpha_context.Script.location * Alpha_context.Script.expr * Script_typed_ir.execution_trace option

type error += Cannot_serialize_failure

type error += Cannot_serialize_storage

type error += Not_support

let cost_of_instr : type a s r f. (a, s, r, f) Script_typed_ir.kinstr -> a -> s -> Alpha_context.Gas.cost =
 fun _i _accu _stack ->
   Alpha_context.Gas.free
 [@@ocaml.inline always]
 [@@coq_axiom_with_reason "unreachable expression `.` not handled"]

let cost_of_control : type a s r f. (a, s, r, f) Script_typed_ir.continuation -> Alpha_context.Gas.cost =
 fun _ks -> Alpha_context.Gas.free

let consume_instr local_gas_counter k accu stack =
  let cost = cost_of_instr k accu stack in
  Local_gas_counter.consume_opt local_gas_counter cost
  [@@ocaml.inline always]

let consume_control local_gas_counter ks =
  let cost = cost_of_control ks in
  Local_gas_counter.consume_opt local_gas_counter cost
  [@@ocaml.inline always]

let rec next:
    type a s r f.
    Local_gas_counter.outdated_context * Script_typed_ir.step_constants ->
    Local_gas_counter.local_gas_counter ->
    (a, s, r, f) Script_typed_ir.continuation ->
    a ->
    s ->
    (r * f * Local_gas_counter.outdated_context * Local_gas_counter.local_gas_counter) tzresult Lwt.t =
 fun ((ctxt, _) as g) gas ks0 accu stack ->
  match consume_control gas ks0 with
  | None -> wrap_tzerror_lwt @@ fail Alpha_context.Gas.Operation_quota_exceeded
  | Some gas -> (
      match ks0 with
      | KNil -> Lwt.return (Ok (accu, stack, ctxt, gas))
      | KCons (k, ks) -> (step [@ocaml.tailcall]) g gas k ks accu stack
      | KReturn (stack', ks) -> (next [@ocaml.tailcall]) g gas ks accu stack'
      | KUndip (x, ks) -> (next [@ocaml.tailcall]) g gas ks x (accu, stack)
      | _ ->
          fail Not_support)

and ifailwith : Chusai_script_interpreter_defs.ifailwith_type =
  let open Tezos_013_PtJakart_test_helpers.Error_monad_operators in
  {
    ifailwith =
      (fun logger (ctxt, _) gas kloc tv accu ->
        let v = accu in
        let ctxt = Local_gas_counter.update_context gas ctxt in
        trace Cannot_serialize_failure (wrap_tztrace_lwt @@ Script_ir_translator.unparse_data ctxt Optimized tv v)
        >>=? fun (v, _ctxt) ->
        let v = Tezos_micheline.Micheline.strip_locations v in
        Chusai_script_interpreter_defs.get_log logger >>=?? fun log -> fail (Reject (kloc, v, log)));
  }

and step : type a s b t r f. (a, s, b, t, r, f) Chusai_script_interpreter_defs.step_type =
 fun ((ctxt, sc) as g) gas i ks accu stack ->
  let open Script_typed_ir in
  let open Tezos_protocol_environment_013_PtJakart.Environment in
  let open Tezos_protocol_013_PtJakart.Protocol.Alpha_context in
  let open Local_gas_counter in
  match consume_instr gas i accu stack with
  | None -> wrap_tzerror_lwt @@ fail Gas.Operation_quota_exceeded
  | Some gas -> (
      match i with
      | ILog (_, _, _, _) ->
          fail Not_support
      | IHalt _ -> (next [@ocaml.tailcall]) g gas ks accu stack
      (* stack ops *)
      | IDrop (_, k) ->
          let (accu, stack) = stack in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IDup (_, k) -> (step [@ocaml.tailcall]) g gas k ks accu (accu, stack)
      | ISwap (_, k) ->
          let (top, stack) = stack in
          (step [@ocaml.tailcall]) g gas k ks top (accu, stack)
      | IConst (_, v, k) -> (step [@ocaml.tailcall]) g gas k ks v (accu, stack)
      (* options *)
      | ICons_some (_, k) ->
          (step [@ocaml.tailcall]) g gas k ks (Some accu) stack
      | ICons_none (_, k) ->
          (step [@ocaml.tailcall]) g gas k ks None (accu, stack)
      | IIf_none {branch_if_none; branch_if_some; k; _} -> (
          match accu with
          | None ->
              let (accu, stack) = stack in
              (step [@ocaml.tailcall])
                g
                gas
                branch_if_none
                (KCons (k, ks))
                accu
                stack
          | Some v ->
              (step [@ocaml.tailcall])
                g
                gas
                branch_if_some
                (KCons (k, ks))
                v
                stack)
      | IOpt_map {body; k; kinfo = _} -> (
          match accu with
          | None -> (step [@ocaml.tailcall]) g gas k ks None stack
          | Some v ->
              let ks' = KMap_head (Option.some, KCons (k, ks)) in
              (step [@ocaml.tailcall]) g gas body ks' v stack)
      (* pairs *)
      | ICons_pair (_, k) ->
          let (b, stack) = stack in
          (step [@ocaml.tailcall]) g gas k ks (accu, b) stack
      | IUnpair (_, k) ->
          let (a, b) = accu in
          (step [@ocaml.tailcall]) g gas k ks a (b, stack)
      | ICar (_, k) ->
          let (a, _) = accu in
          (step [@ocaml.tailcall]) g gas k ks a stack
      | ICdr (_, k) ->
          let (_, b) = accu in
          (step [@ocaml.tailcall]) g gas k ks b stack
      (* unions *)
      | ICons_left (_, k) -> (step [@ocaml.tailcall]) g gas k ks (L accu) stack
      | ICons_right (_, k) -> (step [@ocaml.tailcall]) g gas k ks (R accu) stack
      | IIf_left {branch_if_left; branch_if_right; k; _} -> (
          match accu with
          | L v ->
              (step [@ocaml.tailcall])
                g
                gas
                branch_if_left
                (KCons (k, ks))
                v
                stack
          | R v ->
              (step [@ocaml.tailcall])
                g
                gas
                branch_if_right
                (KCons (k, ks))
                v
                stack)
      (* lists *)
      | ICons_list (_, k) ->
          let (tl, stack) = stack in
          let accu = Script_list.cons accu tl in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | INil (_, k) ->
          let stack = (accu, stack) in
          let accu = Script_list.empty in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IIf_cons {branch_if_cons; branch_if_nil; k; _} -> (
          match accu.elements with
          | [] ->
              let (accu, stack) = stack in
              (step [@ocaml.tailcall])
                g
                gas
                branch_if_nil
                (KCons (k, ks))
                accu
                stack
          | hd :: tl ->
              let tl = {elements = tl; length = accu.length - 1} in
              (step [@ocaml.tailcall])
                g
                gas
                branch_if_cons
                (KCons (k, ks))
                hd
                (tl, stack))
      | IList_map (_, _body, _k) ->
          fail Not_support
      | IList_size (_, k) ->
          let list = accu in
          let len = Script_int.(abs (of_int list.length)) in
          (step [@ocaml.tailcall]) g gas k ks len stack
      | IList_iter (_, _body, _k) ->
          fail Not_support
      (* sets *)
      | IEmpty_set (_, ty, k) ->
          let res = Script_set.empty ty in
          let stack = (accu, stack) in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | ISet_iter (_, _body, _k) ->
          fail Not_support
      | ISet_mem (_, k) ->
          let (set, stack) = stack in
          let res = Script_set.mem accu set in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | ISet_update (_, k) ->
          let (presence, (set, stack)) = stack in
          let res = Script_set.update accu presence set in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | ISet_size (_, k) ->
          let res = Script_set.size accu in
          (step [@ocaml.tailcall]) g gas k ks res stack
      (* maps *)
      | IEmpty_map (_, ty, k) ->
          let res = Script_map.empty ty and stack = (accu, stack) in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMap_map (_, _body, _k) ->
          fail Not_support
      | IMap_iter (_, _body, _k) ->
          fail Not_support
      | IMap_mem (_, k) ->
          let (map, stack) = stack in
          let res = Script_map.mem accu map in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMap_get (_, k) ->
          let (map, stack) = stack in
          let res = Script_map.get accu map in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMap_update (_, k) ->
          let (v, (map, stack)) = stack in
          let key = accu in
          let res = Script_map.update key v map in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMap_get_and_update (_, k) ->
          let key = accu in
          let (v, (map, rest)) = stack in
          let map' = Script_map.update key v map in
          let v' = Script_map.get key map in
          (step [@ocaml.tailcall]) g gas k ks v' (map', rest)
      | IMap_size (_, k) ->
          let res = Script_map.size accu in
          (step [@ocaml.tailcall]) g gas k ks res stack
      (* Big map operations *)
      | IEmpty_big_map (_, tk, tv, k) ->
          let ebm = Script_ir_translator.empty_big_map tk tv in
          (step [@ocaml.tailcall]) g gas k ks ebm (accu, stack)
      | IBig_map_mem (_, _k) ->
          fail Not_support
      | IBig_map_get (_, _k) ->
          fail Not_support
      | IBig_map_update (_, _k) ->
          fail Not_support
      | IBig_map_get_and_update (_, _k) ->
          fail Not_support
      (* timestamp operations *)
      | IAdd_seconds_to_timestamp (_, k) ->
          let n = accu in
          let (t, stack) = stack in
          let result = Script_timestamp.add_delta t n in
          (step [@ocaml.tailcall]) g gas k ks result stack
      | IAdd_timestamp_to_seconds (_, k) ->
          let t = accu in
          let (n, stack) = stack in
          let result = Script_timestamp.add_delta t n in
          (step [@ocaml.tailcall]) g gas k ks result stack
      | ISub_timestamp_seconds (_, k) ->
          let t = accu in
          let (s, stack) = stack in
          let result = Script_timestamp.sub_delta t s in
          (step [@ocaml.tailcall]) g gas k ks result stack
      | IDiff_timestamps (_, k) ->
          let t1 = accu in
          let (t2, stack) = stack in
          let result = Script_timestamp.diff t1 t2 in
          (step [@ocaml.tailcall]) g gas k ks result stack
      (* string operations *)
      | IConcat_string_pair (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          let s = Script_string.concat_pair x y in
          (step [@ocaml.tailcall]) g gas k ks s stack
      | IConcat_string (_, k) ->
          let ss = accu in
          let gas = Local_gas_counter.Local_gas_counter 0 in
          let s = Script_string.concat ss.elements in
          (step [@ocaml.tailcall]) g gas k ks s stack
      | ISlice_string (_, _k) ->
          fail Not_support
      | IString_size (_, k) ->
          let s = accu in
          let result = Script_int.(abs (of_int (Script_string.length s))) in
          (step [@ocaml.tailcall]) g gas k ks result stack
      (* bytes operations *)
      | IConcat_bytes_pair (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          let s = Bytes.cat x y in
          (step [@ocaml.tailcall]) g gas k ks s stack
      | IConcat_bytes (_, k) ->
          let ss = accu in
          let gas = Local_gas_counter.Local_gas_counter 0 in
          let s = Bytes.concat Bytes.empty ss.elements in
          (step [@ocaml.tailcall]) g gas k ks s stack
      | ISlice_bytes (_, _k) ->
          fail Not_support
      | IBytes_size (_, k) ->
          let s = accu in
          let result = Script_int.(abs (of_int (Bytes.length s))) in
          (step [@ocaml.tailcall]) g gas k ks result stack
      (* currency operations *)
      | IAdd_tez (_, _k) ->
          fail Not_support
      | ISub_tez (_, _k) ->
          fail Not_support
      | ISub_tez_legacy (_, _k) ->
          fail Not_support
      | IMul_teznat (_kinfo, _k) ->
          fail Not_support
      | IMul_nattez (_kinfo, _k) ->
          fail Not_support
      (* boolean operations *)
      | IOr (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          (step [@ocaml.tailcall]) g gas k ks (x || y) stack
      | IAnd (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          (step [@ocaml.tailcall]) g gas k ks (x && y) stack
      | IXor (_, k) ->
          let x = accu in
          let (y, stack) = stack in
          let res = Compare.Bool.(x <> y) in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | INot (_, k) ->
          let x = accu in
          (step [@ocaml.tailcall]) g gas k ks (not x) stack
      (* integer operations *)
      | IIs_nat (_, k) ->
          let x = accu in
          let res = Script_int.is_nat x in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IAbs_int (_, k) ->
          let x = accu in
          let res = Script_int.abs x in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IInt_nat (_, k) ->
          let x = accu in
          let res = Script_int.int x in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | INeg (_, k) ->
          let x = accu in
          let res = Script_int.neg x in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IAdd_int (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.add x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IAdd_nat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.add_n x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | ISub_int (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.sub x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMul_int (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.mul x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMul_nat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.mul_n x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IEdiv_teznat (_, _k) ->
          fail Not_support
      | IEdiv_tez (_, _k) ->
          fail Not_support
      | IEdiv_int (_, _k) ->
          fail Not_support
      | IEdiv_nat (_, _k) ->
          fail Not_support
      | ILsl_nat (_kinfo, _k) ->
          fail Not_support
      | ILsr_nat (_kinfo, _k) ->
          fail Not_support
      | IOr_nat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.logor x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IAnd_nat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.logand x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IAnd_int_nat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.logand x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IXor_nat (_, k) ->
          let x = accu and (y, stack) = stack in
          let res = Script_int.logxor x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | INot_int (_, k) ->
          let x = accu in
          let res = Script_int.lognot x in
          (step [@ocaml.tailcall]) g gas k ks res stack
      (* control *)
      | IIf {branch_if_true; branch_if_false; k; _} ->
          let (res, stack) = stack in
          if accu then
            (step [@ocaml.tailcall])
              g
              gas
              branch_if_true
              (KCons (k, ks))
              res
              stack
          else
            (step [@ocaml.tailcall])
              g
              gas
              branch_if_false
              (KCons (k, ks))
              res
              stack
      | ILoop (_, body, k) ->
          let ks = KLoop_in (body, KCons (k, ks)) in
          (next [@ocaml.tailcall]) g gas ks accu stack
      | ILoop_left (_, bl, br) ->
          let ks = KLoop_in_left (bl, KCons (br, ks)) in
          (next [@ocaml.tailcall]) g gas ks accu stack
      | IDip (_, b, k) ->
          let ign = accu in
          let ks = KUndip (ign, KCons (k, ks)) in
          let (accu, stack) = stack in
          (step [@ocaml.tailcall]) g gas b ks accu stack
      | IExec (_, _k) -> 
          fail Not_support
      | IApply (_, _capture_ty, _k) ->
          fail Not_support
      | ILambda (_, lam, k) ->
          (step [@ocaml.tailcall]) g gas k ks lam (accu, stack)
      | IFailwith (_, kloc, tv) ->
          let Chusai_script_interpreter_defs.{ifailwith} = ifailwith in
          ifailwith None g gas kloc tv accu
      (* comparison *)
      | ICompare (_, ty, k) ->
          let a = accu in
          let (b, stack) = stack in
          let r =
            Script_int.of_int @@ Script_comparable.compare_comparable ty a b
          in
          (step [@ocaml.tailcall]) g gas k ks r stack
      (* comparators *)
      | IEq (_, k) ->
          let a = accu in
          let a = Script_int.compare a Script_int.zero in
          let a = Compare.Int.(a = 0) in
          (step [@ocaml.tailcall]) g gas k ks a stack
      | INeq (_, k) ->
          let a = accu in
          let a = Script_int.compare a Script_int.zero in
          let a = Compare.Int.(a <> 0) in
          (step [@ocaml.tailcall]) g gas k ks a stack
      | ILt (_, k) ->
          let a = accu in
          let a = Script_int.compare a Script_int.zero in
          let a = Compare.Int.(a < 0) in
          (step [@ocaml.tailcall]) g gas k ks a stack
      | ILe (_, k) ->
          let a = accu in
          let a = Script_int.compare a Script_int.zero in
          let a = Compare.Int.(a <= 0) in
          (step [@ocaml.tailcall]) g gas k ks a stack
      | IGt (_, k) ->
          let a = accu in
          let a = Script_int.compare a Script_int.zero in
          let a = Compare.Int.(a > 0) in
          (step [@ocaml.tailcall]) g gas k ks a stack
      | IGe (_, k) ->
          let a = accu in
          let a = Script_int.compare a Script_int.zero in
          let a = Compare.Int.(a >= 0) in
          (step [@ocaml.tailcall]) g gas k ks a stack
      (* packing *)
      | IPack (_, ty, k) ->
          let value = accu in
          wrap_tztrace_lwt @@ ( use_gas_counter_in_context ctxt gas @@ fun ctxt ->
            Script_ir_translator.pack_data ctxt ty value )
          >>=? fun (bytes, ctxt, gas) ->
          (step [@ocaml.tailcall]) (ctxt, sc) gas k ks bytes stack
      | IUnpack (_, ty, k) ->
          let bytes = accu in
          wrap_tztrace_lwt @@ ( use_gas_counter_in_context ctxt gas @@ fun ctxt ->
            Chusai_script_interpreter_defs.unpack ctxt ~ty ~bytes )
          >>=? fun (opt, ctxt, gas) ->
          (step [@ocaml.tailcall]) (ctxt, sc) gas k ks opt stack
      | IAddress (_, k) ->
          let (Typed_contract {address; _}) = accu in
          (step [@ocaml.tailcall]) g gas k ks address stack
      | IContract (kinfo, t, entrypoint, k) -> (
          let addr = accu in
          let entrypoint_opt =
            if Entrypoint.is_default addr.entrypoint then Some entrypoint
            else if Entrypoint.is_default entrypoint then Some addr.entrypoint
            else (* both entrypoints are non-default *) None
          in
          match entrypoint_opt with
          | Some entrypoint ->
              let ctxt = update_context gas ctxt in
              wrap_tztrace_lwt @@
              Script_ir_translator.parse_contract_for_script
                ctxt
                kinfo.iloc
                t
                addr.destination
                ~entrypoint
              >>=? fun (ctxt, maybe_contract) ->
              let (gas, ctxt) = local_gas_counter_and_outdated_context ctxt in
              let accu = maybe_contract in
              (step [@ocaml.tailcall]) (ctxt, sc) gas k ks accu stack
          | None -> (step [@ocaml.tailcall]) (ctxt, sc) gas k ks None stack)
      | ITransfer_tokens (_kinfo, _k) ->
          fail Not_support
      | IImplicit_account (_, k) ->
          let key = accu in
          let arg_ty = unit_t in
          let address =
            {
              destination = Contract (Contract.implicit_contract key);
              entrypoint = Entrypoint.default;
            }
          in
          let res = Typed_contract {arg_ty; address} in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IView (_, _view, _k) ->
          fail Not_support
      | ICreate_contract {storage_type = _; code = _; k = _; kinfo = _} ->
          fail Not_support
      | ISet_delegate (_, _k) ->
          fail Not_support
      | IBalance (_, k) ->
          let ctxt = update_context gas ctxt in
          let (gas, ctxt) = local_gas_counter_and_outdated_context ctxt in
          let g = (ctxt, sc) in
          (step [@ocaml.tailcall]) g gas k ks sc.balance (accu, stack)
      | ILevel (_, k) ->
          (step [@ocaml.tailcall]) g gas k ks sc.level (accu, stack)
      | INow (_, k) -> (step [@ocaml.tailcall]) g gas k ks sc.now (accu, stack)
      | IMin_block_time (_, k) ->
          let ctxt = update_context gas ctxt in
          let min_block_time =
            Constants.minimal_block_delay ctxt
            |> Period.to_seconds |> Script_int.of_int64
            (* Realistically the block delay is never negative. *)
            |> Script_int.abs
          in
          let new_stack = (accu, stack) in
          (step [@ocaml.tailcall]) g gas k ks min_block_time new_stack
      | ICheck_signature (_, k) ->
          let key = accu and (signature, (message, stack)) = stack in
          let res = Script_signature.check key signature message in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IHash_key (_, k) ->
          let key = accu in
          let res = Signature.Public_key.hash key in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IBlake2b (_, k) ->
          let bytes = accu in
          let hash = Raw_hashes.blake2b bytes in
          (step [@ocaml.tailcall]) g gas k ks hash stack
      | ISha256 (_, k) ->
          let bytes = accu in
          let hash = Raw_hashes.sha256 bytes in
          (step [@ocaml.tailcall]) g gas k ks hash stack
      | ISha512 (_, k) ->
          let bytes = accu in
          let hash = Raw_hashes.sha512 bytes in
          (step [@ocaml.tailcall]) g gas k ks hash stack
      | ISource (_, k) ->
          let destination : Destination.t = Contract sc.payer in
          let res = {destination; entrypoint = Entrypoint.default} in
          (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
      | ISender (_, k) ->
          let destination : Destination.t = Contract sc.source in
          let res = {destination; entrypoint = Entrypoint.default} in
          (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
      | ISelf (_, ty, entrypoint, k) ->
          let destination : Destination.t = Contract sc.self in
          let address = {destination; entrypoint} in
          let res = Typed_contract {arg_ty = ty; address} in
          (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
      | ISelf_address (_, k) ->
          let destination : Destination.t = Contract sc.self in
          let res = {destination; entrypoint = Entrypoint.default} in
          (step [@ocaml.tailcall]) g gas k ks res (accu, stack)
      | IAmount (_, k) ->
          let accu = sc.amount and stack = (accu, stack) in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IDig (_, _n, n', k) ->
          let ((accu, stack), x) =
            Chusai_script_interpreter_defs.interp_stack_prefix_preserving_operation
              (fun v stack -> (stack, v))
              n'
              accu
              stack
          in
          let accu = x and stack = (accu, stack) in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IDug (_, _n, n', k) ->
          let v = accu in
          let (accu, stack) = stack in
          let ((accu, stack), ()) =
            Chusai_script_interpreter_defs.interp_stack_prefix_preserving_operation
              (fun accu stack -> ((v, (accu, stack)), ()))
              n'
              accu
              stack
          in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IDipn (_, _n, n', b, k) ->
          let (accu, stack, restore_prefix) = Chusai_script_interpreter_defs.kundip n' accu stack k in
          let ks = KCons (restore_prefix, ks) in
          (step [@ocaml.tailcall]) g gas b ks accu stack
      | IDropn (_, _n, n', k) ->
          let stack =
            let rec aux :
                type a s b t.
                (b, t, b, t, a, s, a, s) stack_prefix_preservation_witness ->
                a ->
                s ->
                b * t =
             fun w accu stack ->
              match w with
              | KRest -> (accu, stack)
              | KPrefix (_, w) ->
                  let (accu, stack) = stack in
                  aux w accu stack
            in
            aux n' accu stack
          in
          let (accu, stack) = stack in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | ISapling_empty_state (_, memo_size, k) ->
          let state = Sapling.empty_state ~memo_size () in
          (step [@ocaml.tailcall]) g gas k ks state (accu, stack)
      | ISapling_verify_update (_, _k) ->
          fail Not_support
      | ISapling_verify_update_deprecated (_, _k) ->
          fail Not_support
      | IChainId (_, k) ->
          let accu = Script_chain_id.make sc.chain_id
          and stack = (accu, stack) in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | INever _ -> ( match accu with _ -> .)
      | IVoting_power (_, _k) ->
          fail Not_support
      | ITotal_voting_power (_, _k) ->
          fail Not_support
      | IKeccak (_, k) ->
          let bytes = accu in
          let hash = Raw_hashes.keccak256 bytes in
          (step [@ocaml.tailcall]) g gas k ks hash stack
      | ISha3 (_, k) ->
          let bytes = accu in
          let hash = Raw_hashes.sha3_256 bytes in
          (step [@ocaml.tailcall]) g gas k ks hash stack
      | IAdd_bls12_381_g1 (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Script_bls.G1.add x y in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IAdd_bls12_381_g2 (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Script_bls.G2.add x y in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IAdd_bls12_381_fr (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Script_bls.Fr.add x y in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IMul_bls12_381_g1 (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Script_bls.G1.mul x y in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IMul_bls12_381_g2 (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Script_bls.G2.mul x y in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IMul_bls12_381_fr (_, k) ->
          let x = accu and (y, stack) = stack in
          let accu = Script_bls.Fr.mul x y in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IMul_bls12_381_fr_z (_, k) ->
          let x = accu and (y, stack) = stack in
          let x = Script_bls.Fr.of_z (Script_int.to_zint x) in
          let res = Script_bls.Fr.mul x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IMul_bls12_381_z_fr (_, k) ->
          let y = accu and (x, stack) = stack in
          let x = Script_bls.Fr.of_z (Script_int.to_zint x) in
          let res = Script_bls.Fr.mul x y in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | IInt_bls12_381_fr (_, k) ->
          let x = accu in
          let res = Script_int.of_zint (Script_bls.Fr.to_z x) in
          (step [@ocaml.tailcall]) g gas k ks res stack
      | INeg_bls12_381_g1 (_, k) ->
          let x = accu in
          let accu = Script_bls.G1.negate x in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | INeg_bls12_381_g2 (_, k) ->
          let x = accu in
          let accu = Script_bls.G2.negate x in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | INeg_bls12_381_fr (_, k) ->
          let x = accu in
          let accu = Script_bls.Fr.negate x in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IPairing_check_bls12_381 (_, k) ->
          let pairs = accu in
          let check = Script_bls.pairing_check pairs.elements in
          (step [@ocaml.tailcall]) g gas k ks check stack
      | IComb (_, _, witness, k) ->
          let rec aux :
              type before after.
              (before, after) comb_gadt_witness -> before -> after =
           fun witness stack ->
            match (witness, stack) with
            | (Comb_one, stack) -> stack
            | (Comb_succ witness', (a, tl)) ->
                let (b, tl') = aux witness' tl in
                ((a, b), tl')
          in
          let stack = aux witness (accu, stack) in
          let (accu, stack) = stack in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IUncomb (_, _, witness, k) ->
          let rec aux :
              type before after.
              (before, after) uncomb_gadt_witness -> before -> after =
           fun witness stack ->
            match (witness, stack) with
            | (Uncomb_one, stack) -> stack
            | (Uncomb_succ witness', ((a, b), tl)) -> (a, aux witness' (b, tl))
          in
          let stack = aux witness (accu, stack) in
          let (accu, stack) = stack in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IComb_get (_, _, witness, k) ->
          let comb = accu in
          let rec aux :
              type before after.
              (before, after) comb_get_gadt_witness -> before -> after =
           fun witness comb ->
            match (witness, comb) with
            | (Comb_get_zero, v) -> v
            | (Comb_get_one, (a, _)) -> a
            | (Comb_get_plus_two witness', (_, b)) -> aux witness' b
          in
          let accu = aux witness comb in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IComb_set (_, _, witness, k) ->
          let value = accu and (comb, stack) = stack in
          let rec aux :
              type value before after.
              (value, before, after) comb_set_gadt_witness ->
              value ->
              before ->
              after =
           fun witness value item ->
            match (witness, item) with
            | (Comb_set_zero, _) -> value
            | (Comb_set_one, (_hd, tl)) -> (value, tl)
            | (Comb_set_plus_two witness', (hd, tl)) ->
                (hd, aux witness' value tl)
          in
          let accu = aux witness value comb in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IDup_n (_, _, witness, k) ->
          let rec aux :
              type before after.
              (before, after) dup_n_gadt_witness -> before -> after =
           fun witness stack ->
            match (witness, stack) with
            | (Dup_n_zero, (a, _)) -> a
            | (Dup_n_succ witness', (_, tl)) -> aux witness' tl
          in
          let stack = (accu, stack) in
          let accu = aux witness stack in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      (* Tickets *)
      | ITicket (_, k) ->
          let contents = accu and (amount, stack) = stack in
          let ticketer = sc.self in
          let accu = {ticketer; contents; amount} in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | IRead_ticket (_, k) ->
          let {ticketer; contents; amount} = accu in
          let stack = (accu, stack) in
          let destination : Destination.t = Contract ticketer in
          let addr = {destination; entrypoint = Entrypoint.default} in
          let accu = (addr, (contents, amount)) in
          (step [@ocaml.tailcall]) g gas k ks accu stack
      | ISplit_ticket (_, k) ->
          let ticket = accu and ((amount_a, amount_b), stack) = stack in
          let result =
            if
              Compare.Int.(
                Script_int.(compare (add_n amount_a amount_b) ticket.amount) = 0)
            then
              Some
                ( {ticket with amount = amount_a},
                  {ticket with amount = amount_b} )
            else None
          in
          (step [@ocaml.tailcall]) g gas k ks result stack
      | IJoin_tickets (_, contents_ty, k) ->
          let (ticket_a, ticket_b) = accu in
          let result =
            if
              Compare.Int.(
                Contract.compare ticket_a.ticketer ticket_b.ticketer = 0
                && Script_comparable.compare_comparable
                     contents_ty
                     ticket_a.contents
                     ticket_b.contents
                   = 0)
            then
              Some
                {
                  ticketer = ticket_a.ticketer;
                  contents = ticket_a.contents;
                  amount = Script_int.add_n ticket_a.amount ticket_b.amount;
                }
            else None
          in
          (step [@ocaml.tailcall]) g gas k ks result stack
      | IOpen_chest (_, k) ->
          let open Timelock in
          let chest_key = accu in
          let (chest, (time_z, stack)) = stack in
          (* If the time is not an integer we then consider the proof as
             incorrect. Indeed the verification asks for an integer for practical reasons.
             Therefore no proof can be correct.*)
          let accu =
            match Script_int.to_int time_z with
            | None -> R false
            | Some time -> (
                match Script_timelock.open_chest chest chest_key ~time with
                | Correct bytes -> L bytes
                | Bogus_cipher -> R false
                | Bogus_opening -> R true)
          in
          (step [@ocaml.tailcall]) g gas k ks accu stack)

(*

   Entrypoints
   ===========

*)

let step_descr ~log_now logger (ctxt, sc) descr accu stack =
  let open Local_gas_counter in
  let open Script_typed_ir in
  let (gas, outdated_ctxt) = local_gas_counter_and_outdated_context ctxt in
  (match logger with
  | None -> step (outdated_ctxt, sc) gas descr.kinstr KNil accu stack
  | Some logger ->
      (if log_now then
       let kinfo = kinfo_of_kinstr descr.kinstr in
       logger.log_interp descr.kinstr ctxt kinfo.iloc descr.kbef (accu, stack)) ;
      let log =
        ILog (kinfo_of_kinstr descr.kinstr, LogEntry, logger, descr.kinstr)
      in
      step (outdated_ctxt, sc) gas log KNil accu stack)
  >>=? fun (accu, stack, ctxt, gas) ->
  return (accu, stack, update_context gas ctxt)

let interp logger g (Script_typed_ir.Lam (code, _)) arg =
  step_descr ~log_now:true logger g code arg (EmptyCell, EmptyCell)
  >|=? fun (ret, (EmptyCell, EmptyCell), ctxt) -> (ret, ctxt)
