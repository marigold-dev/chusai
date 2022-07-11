#import "../../stdlib_ext/src/stdlibext.mligo" "Stdlib"

type hash = bytes
type index = nat
type batch = 
    {  parent : index
    ;  level : nat
    ;  hash : hash
    ;  proposer : address
    }
type chain = 
    {   max_index : nat
    ;   batches : (index, batch) big_map
    ;   children : (index, index list) big_map
    }

    
let get_batch (index, store : index * chain) : batch option = Big_map.find_opt index store.batches
let get_children (index, store : index * chain) : index list option = Big_map.find_opt index store.children

let is_batch_valid (batch, store : batch * chain) : bool = 
    batch.parent = 0n ||
    ( let parent_opt = get_batch (batch.parent, store) in
    match parent_opt with
    | None -> false
    | Some parent -> parent.level < batch.level )

let store_batch (batch, chain : batch * chain) : chain =
    let add_to_children (newborn, parent, chain : index * index * chain) : (index, index list)  big_map  =
        let new_children = 
            match get_children (parent, chain) with
            | None ->  [newborn]
            | Some sibligns -> newborn :: sibligns
        in
        Big_map.update parent (Some new_children) chain.children
    in
    if is_batch_valid (batch,chain) then
        let new_index = chain.max_index + 1n in
        { chain with 
              batches = Big_map.update new_index (Some batch) chain.batches 
            ; max_index =  new_index
            ; children = add_to_children (new_index, batch.parent, chain)
        }
    else failwith "could not store invalid batch"

let get_latest_batch (store : chain) : batch =
    match get_batch (store.max_index, store) with
    | None -> failwith "no batch"
    | Some b -> b

let remove_batch (index, chain: index * chain) : chain =
    let delete_batch (chain, index: chain * index) : chain =
        {chain with batches = Big_map.update index (None : batch option) chain.batches} 
    in
    let rec aux (to_delete, chain : index list * chain) : chain = 
        match to_delete with
        | [] -> chain
        | i::q -> 
            begin
            let children = Big_map.find_opt i chain.children in
            let chain = delete_batch (chain, i) in
            match children with
            | None -> aux (q, chain)
            | Some children -> aux (Stdlib.ListExt.concat children q, chain)
            end
    in
    aux ([index],chain)

let find_latest_existing (chain : chain) : batch option = 
    let rec find_existing (index : index) : batch option =
        if index = 0n then (None : batch option)
        else match Big_map.find_opt index chain.batches with
            | None -> find_existing (abs(index - 1n))
            | Some batch -> Some batch
    in
    let max_index = chain.max_index in
    find_existing max_index
