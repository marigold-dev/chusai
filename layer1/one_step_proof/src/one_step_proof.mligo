#include "../../commons/one_step_proof_interface.mligo"
#import "../../stdlib_ext/src/result.mligo" "Stdlib_Result"

(*
  TODO: support balance
*)


module Result = struct

  type error = Wrong_owner | Deserialization_failed | Fail_to_apply_state

  type result = Stdlib_Result.t

  type t = (bool, error) result

  let get_ok_or_raises : t -> string -> bool = Stdlib_Result.get_ok_or_raises
end


let one_step_proof (contract, b_state, b_expected_state : chusai_contract * chusai_state * expected_chusai_state) : Result.t =
      let opt_state = (Bytes.unpack b_state : chusai_entity_state option) in
      match opt_state with
      | None -> Error Deserialization_failed
      | Some (addr, chusai_state, num_ops) -> (
        let ops, opt_chusai_storage = contract chusai_state in
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
      "one_step_proof: couldn't check expected state"
  in
  ([], {owner = storage.owner; is_expected_state = check_expected_state})
