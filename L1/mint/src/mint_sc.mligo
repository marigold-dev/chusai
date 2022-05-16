#include "../../commons/mint_part.mligo"

(* CONFIGURATION *)
(*
    Configuration of the mint.
    Could probably be in a module or smthg. 
    Should be parametrized in any case, but I'm not sure how.
*)

(* a tax rate for amount kept during withdrawal, in %*)
let tax_rate = 15n // in %
(* the payload, fixed for the mint *)
let chusai_payload = 0x00
(* the ticketer, checked for withdrawal *)
let chusai_ticketer () = Tezos.self_address
(* minimum amount accepted by the mint. *)
let minimum_amount = 1tez

(* XTZ <-> CHUSAI*)
let xtz_to_chusai (xtz:tez) = xtz /1mutez
let chusai_to_xtz (chusai_amount:nat) = 
    let n = (chusai_amount * abs(100n - tax_rate)) / 100n in
    n * 1mutez 


(* MINTING *)
let create_chusai () : chusai_ticket=
    let n : nat = xtz_to_chusai Tezos.amount  in
    Tezos.create_ticket chusai_payload n

let mint (chusai_ticket_contr: chusai_ticket contract) : operation list = 
    let _check = if( Tezos.amount < minimum_amount) then failwith "mint_sc : no ticket for less than ..." in    
    let ticket = create_chusai () in
    let op = Tezos.transaction ticket 0tez chusai_ticket_contr in
    [op]

(* REDEEMING *)
(* checks the ticket's kind, and refuses 0-value tickets *)
let redeem (ticket, unit_callback:chusai_ticket * unit contract) : operation list = 
    let (addr, (payload, total)), _ticket = Tezos.read_ticket ticket in
    let _check = if( addr <> (chusai_ticketer ())) then failwith "mint_sc : wrong ticketer" in   
    let _check = if( payload <> chusai_payload) then failwith "mint_sc : wrong payload" in
    let _check = if( total = 0n) then failwith "mint_sc : 0-value ticket" in
    let op = Tezos.transaction unit (chusai_to_xtz total ) unit_callback in
    [op]

(* ENDPOINTS *)
let main (action, _store : mint_parameter * unit) : operation list * unit = 
    (match action with
          Mint chusai_ticket_contr -> mint chusai_ticket_contr
        | Redeem (ticket, unit_callback) -> redeem (ticket, unit_callback))
    , ()


