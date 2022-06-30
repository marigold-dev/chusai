type storage =
    (* [FIXME]: referee's address *)
  { owner : address
    (* [FIXME]: support multiple game *)
  ; is_expected_state : bool
  }


type expected_chusai_state = bytes

type chusai_storage = bytes
type chusai_op_emission = nat
type chusai_contract = (bytes -> (operation list * bytes option))

type chusai_state = bytes
type chusai_entity_state = (address * chusai_storage * chusai_op_emission)

type one_step_proof = chusai_contract * chusai_state * expected_chusai_state

type parameter = 
  | One_step_proof of one_step_proof

type return = operation list * storage
