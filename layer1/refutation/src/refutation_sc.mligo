#include "../../commons/chusai_ticket_interface.mligo"
#include "../../commons/refutation_interface.mligo"


let main (_action, store : refutation_parameter * storage) : operation list * storage = 
    match _action with
          _ -> [], store