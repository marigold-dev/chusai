(*
    This smart contract is used only as a placeholder to try the /chain/ library. 
*)

#include "chain.mligo"
#import "../../stdlib_ext/src/result.mligo" "Result"

type chain_storage = chain
type chain_parameter = 
    | Receive of block_proposal
    | Remove  of index // only provided for test. In bridge, removal only happens at the end of a refutation (or if sibling is finalized ?)
    | Finalize

let reward (block, chain : block * chain) : operation list =
    match (Tezos.get_contract_opt block.proposer : unit contract option) with
    | None -> 
        // could not find proposer, but we don't want to fail finalization, so just return no op (and dont fail)
        []
    | Some winner_contract -> 
        // reward         
        [Tezos.transaction () chain.bond_amount winner_contract]



let apply_finalize (store : chain_storage) : operation list * chain_storage = 
    match get_finalization_candidate store with
    | Error e -> 
        failwith ("Error during finalization:" ^ (pp_chain_error e))
    | Ok candidate -> 
            if is_old_enough (candidate, store, Tezos.now) then
                reward (candidate, store), finalize (candidate.index, store)
            else 
                failwith "Error during finalization: finality period not finished"

let apply_receive (proposal, store : block_proposal * chain_storage) : operation list * chain_storage =
        // recolt bond
        if Tezos.amount < store.bond_amount then 
            failwith "not enough bond"
        else
        // store block
        begin 
            let new_store = increase_index store in
            let block = make_block (proposal, new_store.max_index, Tezos.source, Tezos.now) in
            match store_block (block, new_store) with
            | Error _ -> 
                failwith "could not store"
            | Ok c -> 
                 ([] : operation list) , c 
        end

(* ENDPOINTS *)
let main (action, store : chain_parameter * chain_storage) : operation list * chain_storage = 
    match action with
    | Receive b -> apply_receive (b, store)
    | Finalize -> apply_finalize store
    | Remove i -> [] , remove_block (i, store)

(* VIEWS *) 

[@view] let get_chain ((),s: unit * chain_storage) : chain_storage = s
[@view] let get_latest ((),s: unit * chain_storage) : block option = find_latest_existing s
[@view] let get_next_finalization_candidate ((), s : unit * chain_storage) : block option =
    match get_finalization_candidate s with
    | Error e -> None
    | Ok b -> Some b
