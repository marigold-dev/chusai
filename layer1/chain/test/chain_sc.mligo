(*
    This smart contract is used only as a placeholder to try the /chain/ library. 
*)

// #import "chain.mligo" "Chain_lib"
#import "../../stdlib_ext/src/result.mligo" "Result"
#import "../../stdlib_ext/src/stdlibext.mligo" "Stdlib_ext"
#import "../src/chain_endpoints.mligo" "Chain"

type chain_storage = Chain.chain
type chain_parameter = 
    | Receive of Chain.block_proposal
    | Remove  of Chain.index // only provided for test. In bridge, removal only happens at the end of a refutation (or if sibling is finalized ?)
    | Finalize
    | Withdraw


(* ENDPOINTS *)
let main (action, store : chain_parameter * chain_storage) : operation list * chain_storage = 
    match action with
    | Receive b -> Chain.Endpoints.apply_receive (b, store)
    | Finalize -> Chain.Endpoints.apply_finalize store
    | Remove i -> Chain.Endpoints.apply_remove (i, store)
    | Withdraw -> Chain.Endpoints.apply_withdraw store

(* VIEWS *) 

[@view] let get_latest = Chain.Views.get_latest
[@view] let get_next_finalization_candidate = Chain.Views.get_next_finalization_candidate
