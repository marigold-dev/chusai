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
let originate_full (type a b) (main : a * b -> operation list * b ) (storage : b) (bal : tez) (log : string) : (a, b) originated =
  let my_taddr, _, _ = Test.originate main storage bal in
  let my_contr = Test.to_contract my_taddr in
  let my_addr = Tezos.address my_contr in
  let _ = log_ (log, storage, bal, my_addr) in
  {originated_typed_address = my_taddr ; originated_contract = my_contr ; originated_address = my_addr}
