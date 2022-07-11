type hash = bytes
type index = nat
type batch = 
    {  parent : index
    ;  level : nat
    ;  hash : hash
    ;  proposer : address
    }
type chain_storage = 
    {   max_index : nat
    ;   batches : (index, batch) map
    }

type chain_parameter = 
    | Receive of batch
    
let get_batch (index, store : index * chain_storage) : batch option = Map.find_opt index store.batches

let is_batch_valid (batch, store : batch * chain_storage) : bool = 
    batch.parent = 0n ||
    ( let parent_opt = get_batch (batch.parent, store) in
    match parent_opt with
    | None -> false
    | Some parent -> parent.level < batch.level )

let store_batch (batch, store : batch * chain_storage) : chain_storage =
    if is_batch_valid (batch,store) then
        let new_index = store.max_index + 1n in
        { store with 
              batches = Map.update new_index (Some batch) store.batches 
            ; max_index =  new_index
        }
    else failwith "could not store invalid batch"

let get_latest_batch (store : chain_storage) : batch =
    match get_batch (store.max_index, store) with
    | None -> failwith "no batch"
    | Some b -> b

(* ENDPOINTS *)
let main (action, store : chain_parameter * chain_storage) : operation list * chain_storage = 
    match action with
    | Receive b -> [] , store_batch (b, store)

(* VIEWS *)

[@view] let get_chain ((),s: unit * chain_storage) : chain_storage = s
[@view] let get_latest ((),s: unit * chain_storage) : batch = get_latest_batch s