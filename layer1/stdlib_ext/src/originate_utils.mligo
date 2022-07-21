(* *********************************** *)
(* Origination functions *)
(*
    Defines a polymorphic origination function that wrap the result in a record containing 
    the different usefull values (typed address, and corresponding contract and address)
*)
let debug = false
let log_ (type a) (msg : a) = if debug then Test.log msg
(* ORIGINATED *)
(* a record with multiple informations on a contract *)
type ('a, 'b) originated = {
  originated_typed_address : ('a, 'b) typed_address
; originated_contract : 'a contract
; originated_address : address
}
(* 'polymorphic' origination : 
    let my_originated = originate_full (main,storage,balance,"My Contract") in
    ... my_originated.taddr ... is typed_address
    ... my_originated.addr ... is address
    ... my_originated.contr ... is contract
*)

let originate_full (type a b ticket_val) (main : a * b -> operation list * b ) (ticket_info : ticket_val * nat)  (mk_storage : ticket_val ticket -> b) (log : string) : (a, b) originated =
  let originated_address = Proxy_ticket.originate ticket_info mk_storage main in
  let originated_typed_address = (Test.cast_address originated_address : (a, b) typed_address) in
  let originated_contract = (Test.to_contract originated_typed_address : a contract) in
  let _ = log_ (log, originated_address) in
  { originated_typed_address = originated_typed_address
  ; originated_contract = originated_contract 
  ; originated_address = originated_address
  }