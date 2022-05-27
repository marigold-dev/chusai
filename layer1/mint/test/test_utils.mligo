
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
type  ('a, 'b) originated = {  
    taddr : ('a, 'b) typed_address ; 
    contr : 'a contract ; 
    addr : address
}
(* 'polymorphic' origination : 
    let my_originated = originate_full (main,storage,balance,"My Contract") in
    ... my_originated.taddr ... is typed_address
    ... my_originated.addr ... is contract
    ... my_originated.contr ... is address
*)
let originate_full (type a b) (main, storage, bal, log :  (a * b -> operation list * b ) * b * tez*string) : (a, b) originated = 
  let my_taddr, _, _ = Test.originate main storage bal in
  let my_contr = Test.to_contract my_taddr in
  let my_addr = Tezos.address my_contr in
  let _ = log_ (log, storage, bal, my_addr) in
  {taddr = my_taddr ; contr = my_contr ; addr = my_addr}



(* *********************************** *)
(* TICKET COMPARAISON *)

(* a record containing informations on a ticket. 
    - Values set at "None" are not checked. 
    - Only possible payload is "bytes" (so only useful for `bytes ticket`)
    the reason to fix the payload is that we need to provide the comparaison function for each type of value
    and who cares about non bytes payload ! amiright ? hu ? hu ?
 *)
type ticket_asserts = {
  addr : address option;
  payload : bytes option;
  amount_ : nat option
  }

(* a record with no assertion, 
    can be used for functionnal update, e.r. {no_assert with amount_ = Some 10n }
*)
let no_assert : ticket_asserts = {addr = None ; payload = None ; amount_ = None }

(* polymorphic asserts executed only if not None *)
let assert_with_errors_opt (type a) (equal : (a * a) -> bool) (expected : a option) (actual : a) (msg : string) =
    match expected with
    | None -> ()
    | Some v -> assert_with_error (equal (v, actual)) msg

(* check that a ticket is conform to a set of assertion *)
let check_ticket (asserts : ticket_asserts) (t : chusai_ticket) = 
    let (addr, (payload, total)), ticket = read_ticket t in
    begin
        assert_with_errors_opt (fun (a, b : address * address) -> a=b)   asserts.addr addr "compare_tickets : wrong address";
        assert_with_errors_opt (fun (a, b : bytes * bytes) -> a=b)   asserts.payload payload "compare_tickets : wrong payload";
        assert_with_errors_opt (fun (a, b : nat * nat) -> a=b)   asserts.amount_  total"compare_tickets : wrong amount";
        ticket
    end

