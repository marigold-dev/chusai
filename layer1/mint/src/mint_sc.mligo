#import "../../commons/ticket/chusai_ticket.mligo" "Ticket"
#include "../../commons/mint_interface.mligo"

(* CONFIGURATION *)
(*
    Configuration of the mint.
    Parametrized value are in storage : they should be fixed at origination
*)
type storage = [@layout:comb] {
    (* the payload, fixed for the mint *)
    payload : Ticket.payload;
    (* minimum amount accepted by the mint. *)
    minimum_amount : tez
}

(* a tax rate for amount kept during withdrawal, in %*)
let tax_rate = 15n // %

(* XTZ <-> Chusai ticket amount*)
let xtz_to_chusai_amount (xtz : tez) : nat = xtz /1mutez
let chusai_amount_to_xtz (chusai_amount : nat) : tez = chusai_amount * 1mutez 


(* MINTING *)
let create_chusai_ticket (payload : Ticket.payload) : Ticket.t =
    let n : nat = xtz_to_chusai_amount Tezos.amount  in
    Ticket.create_ticket Tezos.self_address payload n

let mint (chusai_ticket_contr, store :Ticket.t contract * storage) : operation list =
    let _check = if Tezos.amount < store.minimum_amount then failwith "mint_sc : no ticket for less than minimum amount" in
    let ticket = create_chusai_ticket store.payload in
    let op = Tezos.transaction ticket 0tez chusai_ticket_contr in
    [op]

(* REDEEMING *)
(* checks the ticket's kind, and refuses 0-value tickets *)
let redeem (ticket, unit_callback, store :Ticket.t * unit contract * storage) : operation list =
    let (addr, (payload, total)), _ticket = Ticket.read_ticket ticket in
    let _check = if total = 0n then failwith "mint_sc : 0-value ticket" in
    let _check = if addr <> Tezos.self_address then failwith "mint_sc : wrong ticketer" in
    let _check = if payload <> store.payload then failwith "mint_sc : wrong payload" in
    let op = Tezos.transaction unit (chusai_amount_to_xtz total) unit_callback in
    [op]

(* ENDPOINTS *)
let main (action, store : mint_parameter * storage) : operation list * storage = 
    (match action with
          Mint chusai_ticket_contr -> mint (chusai_ticket_contr, store)
        | Redeem (ticket, unit_callback) -> redeem (ticket, unit_callback, store))
    , store
