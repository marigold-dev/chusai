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
    
let is_batch_valid (batch, store : batch * chain_storage) : bool = true

let store_batch (batch, store : batch * chain_storage) : chain_storage =
    if is_batch_valid (batch,store) then
        {store with 
              batches = Map.update store.max_index (Some batch) store.batches 
            ; max_index = store.max_index + 1n 
        }
    else store

let get_latest_batch (store : chain_storage) : batch =
    match Map.find_opt store.max_index store.batches with
    | None -> failwith "no batch"
    | Some b -> b

(* ENDPOINTS *)
let main (action, store : chain_parameter * chain_storage) : operation list * chain_storage = 
    match action with
    | Receive b -> [] , store_batch (b, store)

[@view] let get_chain ((),s: unit * chain_storage) : chain_storage = s
[@view] let get_latest ((),s: unit * chain_storage) : batch = get_latest_batch s