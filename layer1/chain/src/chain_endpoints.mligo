
#include "chain.mligo"
#import "../../stdlib_ext/src/result.mligo" "Result"
#import "../../stdlib_ext/src/stdlibext.mligo" "Stdlib_ext"

module Endpoints = struct
let reward (block, chain : block * chain) : operation list =
    match (Tezos.get_contract_opt block.proposer : unit contract option) with
    | None -> 
        // could not find proposer, but we don't want to fail finalization, so just return no op (and dont fail)
        []
    | Some winner_contract -> 
        // reward         
        [Tezos.transaction () chain.bond_amount winner_contract]



let apply_finalize (store : chain) : operation list * chain = 
    match get_finalization_candidate store with
    | Error e -> 
        failwith ("Error during finalization:" ^ (pp_chain_error e))
    | Ok candidate -> 
            if is_old_enough (candidate, store, Tezos.now) then
                reward (candidate, store), finalize (candidate, store)
            else 
                failwith "Error during finalization: finality period not finished"

let apply_receive (proposal, store : block_proposal * chain) : operation list * chain =
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

(* removes a block, making sure to compensate every body who proposed a block based on the removed on (recursively) 
   /!\ this is for test. Depending on the situation, a proper removal (after refutation) might use a more complexe reimbursement strategy
*)
let apply_remove (i, store : index * chain) =
    let rec reward_deleted (ops, blocks, store : operation list * block list * chain) : operation list = 
        match blocks with
        | [] -> ops
        | b :: q -> 
            let reward_ops = reward (b, store) in
            let new_ops = Stdlib_ext.ListExt.concat reward_ops ops in
            reward_deleted (new_ops, q, store)
    in
    let blocks, chain = remove_block (i, store) in
    let ops = reward_deleted (([] : operation list) , blocks, chain) in
    ops, chain
end

module Views = struct

    let get_latest ((),s: unit * chain) : block option = find_latest_existing s
    let get_next_finalization_candidate ((), s : unit * chain) : block option =
        match get_finalization_candidate s with
        | Error e -> None
        | Ok b -> Some b
end