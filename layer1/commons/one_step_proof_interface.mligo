type storage =
    (* [FIXME]: referee's address *)
  { owner : address
    (* [FIXME]: support multiple game *)
  ; is_expected_state : bool
  }


type expected_chusai_state = bytes

type chusai_storage = bytes option
type chusai_arg = bytes
type chusai_op_emission = nat
type chusai_contract = bytes

type chusai_state = bytes
type chusai_entity_contract = (chusai_arg * chusai_storage) -> (operation list * chusai_storage)
type chusai_entity_state = (address * chusai_storage * chusai_op_emission)

type one_step_proof = chusai_contract * chusai_arg * chusai_state * expected_chusai_state

type parameter = 
  | One_step_proof of one_step_proof

type return = operation list * storage
