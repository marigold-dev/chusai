#include "chain.mligo"
type chain_storage = chain
type chain_parameter = 
    | Receive of batch

(* ENDPOINTS *)
let main (action, store : chain_parameter * chain_storage) : operation list * chain_storage = 
    match action with
    | Receive b -> 
        begin 
        match store_batch (b, store) with
        | None -> failwith "could not store"
        | Some c -> [] , c
        end


(* VIEWS *)

[@view] let get_chain ((),s: unit * chain_storage) : chain_storage = s
[@view] let get_latest ((),s: unit * chain_storage) : batch option = find_latest_existing s