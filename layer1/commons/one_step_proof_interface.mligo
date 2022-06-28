type storage =
    (* [FIXME]: referee's address *)
  { owner : address
    (* [FIXME]: support multiple game *)
  ; is_expected_state : bool
  }

type state = bytes
type expected_state = bytes

type chusai_storage = bytes
type chusai_contract = (bytes -> (operation list * bytes option))

type entity_state =  (address * chusai_storage * nat)


type parameter = chusai_contract * state * expected_state
type return = operation list * storage
