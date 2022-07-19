(*
    This smart contract is used only as a placeholder to try the /chain/ library. 
*)

#include "chain.mligo"
type chain_storage = chain
type chain_parameter = 
    | Receive of batch
    | Remove  of index

(* ENDPOINTS *)
let main (action, store : chain_parameter * chain_storage) : operation list * chain_storage = 
    match action with
    | Receive b -> 
        begin 
        match store_batch (b, store) with
        | Error _ -> failwith "could not store"
        | Ok c -> [] , c
        end
    | Remove i -> [] , remove_batch (i, store)


(* VIEWS *)

[@view] let get_chain ((),s: unit * chain_storage) : chain_storage = s
[@view] let get_latest ((),s: unit * chain_storage) : batch option = find_latest_existing s