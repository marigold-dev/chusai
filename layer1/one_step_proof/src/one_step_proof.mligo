#include "../../commons/one_step_proof_interface.mligo"
#import "../../stdlib_ext/src/result.mligo" "Stdlib_Result"

module Result = struct

  type error = Wrong_owner | Deserialization_failed | Fail_to_apply_state

  type result = Stdlib_Result.t

  type t = (bool, error) result

  let get_ok_or_raises : t -> string -> bool = Stdlib_Result.get_ok_or_raises
end

let main((contract, b_state, b_expected_state), storage : parameter * storage) : return =
  let check_expected_state =
    Result.get_ok_or_raises (
      if storage.owner <> Tezos.get_sender () then (Error Wrong_owner : Result.t)
      else
        let opt_state = (Bytes.unpack b_state : entity_state option) in
        match opt_state with
        | None -> Error Deserialization_failed
        | Some (addr, chusai_state, num_ops) -> (
          let ops, opt_chusai_storage = contract chusai_state in
          match opt_chusai_storage with
          | None -> Error Fail_to_apply_state
          | Some new_chusai_state -> (
               let b_new_state = Bytes.pack (addr, new_chusai_state, List.size ops) in
               if b_new_state = b_expected_state then Ok true else Ok false
              )))
      "one_step_proof: couldn't check expected state"
  in
  ([], {owner = storage.owner; is_expected_state = check_expected_state})

(*

type storage = bytes option
type state = bytes
type parameter = bytes * state

type return = storage

let lambda (b_input : bytes) =
  let o_input = (Bytes.unpack b_input : nat option) in
  match o_input with
  | None -> None
  | Some input -> (
    let res = input + 1 in
    let b_out = Bytes.pack res in
    Some b_out)

 
let main((f, state), storage : parameter * storage) : return =
    let opt_f = (Bytes.unpack f : (bytes -> bytes option) option) in
    match opt_f with
    | None -> failwith "fail"
    | Some f ->
       let new_state = f state in
       (new_state)
*)

(*
let main((f, state), storage : parameter * storage) : return =
    let new_state = f state in
    ([],new_state)
*)
