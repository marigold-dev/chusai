#include "../../commons/one_step_proof_interface.mligo"
#import "../../stdlib_ext/src/result.mligo" "Stdlib_Result"

(*
  TODO: support balance
*)


module Result = struct
  type error
    = Wrong_owner
    | Contract_deserialization_failed
    | State_deserialization_failed
    | Fail_to_apply_state

  type result = Stdlib_Result.t

  type t = (bool, error) result

  let get_ok_or_raises : t -> (error -> string) -> bool = Stdlib_Result.get_ok_or_raises

  let error_to_string (e : error)
    = match e with
    | Wrong_owner -> "Wrong_owner"
    | Contract_deserialization_failed -> "Contract_deserialization_failed"
    | State_deserialization_failed -> "State_deserialization_failed"
    | Fail_to_apply_state -> "Fail_to_apply_state"
end


let one_step_proof (b_contract, b_arg, b_state, b_expected_state : one_step_proof) : Result.t =
      let opt_contract = (Bytes.unpack b_contract : chusai_entity_contract option) in
      let opt_state = (Bytes.unpack b_state : chusai_entity_state option) in
      match opt_contract, opt_state with
      | None, _ -> Error Contract_deserialization_failed
      | _, None -> Error State_deserialization_failed
      | Some contract, Some (addr, chusai_storage, _num_ops) -> (
        let ops, opt_chusai_storage = contract (b_arg, chusai_storage) in
        match opt_chusai_storage with
        | None -> Error Fail_to_apply_state
        | Some new_chusai_state -> (
             let b_new_state = Bytes.pack (addr, new_chusai_state, List.size ops) in
             if b_new_state = b_expected_state then Ok true else Ok false
            ))

let main(action, storage : parameter * storage) : return =
  let check_expected_state =
    Result.get_ok_or_raises (
      if storage.owner <> Tezos.get_sender () then (Error Wrong_owner : Result.t)
      else
        match action with
        | One_step_proof o -> one_step_proof o)
      Result.error_to_string
  in
  ([], {owner = storage.owner; is_expected_state = check_expected_state})
