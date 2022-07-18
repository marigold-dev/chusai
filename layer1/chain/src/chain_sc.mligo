(*
    This smart contract is used only as a placeholder to try the /chain/ library. 
*)

#include "chain.mligo"
#import "../../stdlib_ext/src/result.mligo" "Result"

type chain_storage = chain
type chain_parameter = 
    | Receive of block
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
    | None -> 
        failwith "no candidate for finalization"
    | Some candidate_index -> 
        begin 
        match get_block (candidate_index,store) with
        | None -> 
            failwith "could not find candidate"
        | Some candidate ->  
            begin 
            if is_old_enough (candidate, store, Tezos.now) then
                reward (candidate, store), finalize (candidate_index, store)
            else 
                failwith "finality period not finished"
            end
        end

let apply_receive (block, store : block * chain_storage) : operation list * chain_storage =
        // recolt bond
        if Tezos.amount < store.bond_amount then 
            failwith "not enough bond"
        else
        // store block
        begin 
            match store_block (block, store) with
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

[@view] let get_latest ((),s: unit * chain_storage) : block option = find_latest_existing s
[@view] let get_next_finalization_candidate ((), s : unit * chain_storage) : block option =
    match get_finalization_candidate s with
    | None -> None
    | Some i -> get_block (i,s)
