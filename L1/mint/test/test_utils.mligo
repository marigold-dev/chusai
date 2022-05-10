
let debug = false
let log_ (type a) (msg:a) = if debug then Test.log msg
(* ORIGINATED *)
(* a record with multiple informations on a contract *)
type  ('a , 'b) originated = {  
    taddr : ('a,'b) typed_address ; 
    contr : 'a contract ; 
    addr : address
}
(* polymorphic origination : 
    let my_originated = originate_full (main,storage,balance,"My Contract") in
    ... my_originated.taddr ... is typed_address
    ... my_originated.addr ... is contract
    ... my_originated.contr ... is address
*)
let originate_full (type a b) (main,storage,bal,log :  (a * b -> operation list * b ) * b * tez*string) : (a,b) originated = 
  let my_taddr, _, _ = Test.originate main storage bal in
  let my_contr = Test.to_contract my_taddr in
  let my_addr = Tezos.address my_contr in
  let _ = log_ (log,storage,bal,my_addr) in
  {taddr = my_taddr ; contr = my_contr ; addr = my_addr}


(* EXECUTION RESULT PROPAGATION *)
(*
    Test.transfer_to_contract returns a "test_exec_result" indicating if the operation failed or succeeded

    We provide a few functions to chain operation and propagate Fail and Success accordingly
*)
let init_result = Success 0n

(* wrap a message in a "Fail" statement *)
let fail (msg:string) = Fail (Other msg)

(* "transfer_to_contract" wraps Test.transfer_to_contract such that it is only applied if the previous operation resulted in Success *)
let transfer_to_contract (type a) (contr: a contract) (param : a) (amount_:tez) (previous:test_exec_result) =
  match previous with
    | Success _ -> Test.transfer_to_contract contr param amount_ 
    | Fail _ -> previous 

(* "assert_" wraps an assertion in Fail or Success, or propagate previous Fail *)
let assert_ (b:bool) (msg:string) (previous:test_exec_result) = 
  match previous with
    | Fail _ -> 
        begin
        log_ ("assert_ : Fail was propagated ",previous);
        previous 
        end
    | Success _ -> if b then Success 0n else fail msg

let is_none (type a) (opt:a option) =
    match opt with
        | None -> true
        | Some _t -> false

(* "assert_none_" wraps an assertion in Fail (not None) or Success (is None) , or propagate previous Fail *)
let assert_none_ (type a) (opt:a option) (msg:string) (previous:test_exec_result) = 
    assert_ (is_none opt) msg previous

(* "assert_rejected_at" check that the result is a Fail
    of type Rejected, emitted at a certain address, or propagates a Fail *)
let assert_rejected_at (at:address) (msg:string) (result:test_exec_result) =
    match result with
        | Success _ -> 
            begin
            log_ ("assert_rejected_at : previous action was not Rejected (but a Success)");
            fail msg
            end
        | Fail (Rejected (_,contr_rejecting)) -> 
            if at = contr_rejecting then Success 0n
            else
                begin 
                log_ (("assert_rejected_at : wrong address -> "^msg),result);
                fail ("assert_rejected_at : wrong address -> "^msg)
                end
        | _ -> 
            begin
            log_ ("assert_rejected_at : Fail was propagated ",result);
            result
            end

(* interrupt if last action was a fail, and replace fail message *)
let assert_is_ok (msg:string) (previous:test_exec_result) =
  match previous with
    | Fail _ -> 
        begin
        log_ ("assert_is_ok : was not ok, there was a fail ",previous);
        fail ("assert_is_ok : "^msg)
        end
    | Success _ -> previous


(* TICKET COMPARAISON *)
(* a record containing informations on a ticket. Values set at "None" are not checked. Only possible payload is "bytes" *)
(* the reason to fix the payload is that we need to provide the comparaison function for each type of value *)
type ticket_asserts = {
  addr : address option;
  payload : bytes option;
  amount_ : nat option
  }
let no_assert : ticket_asserts = {addr = None ; payload = None ; amount_ = None }
(* asserts executed only if not None *)
let assert_with_errors_opt (type a) (equal:(a*a) -> bool) (expected:a option) (actual:a) (msg:string) =
    match expected with
        | None -> ()
        | Some v -> assert_with_error (equal (v,actual)) msg

(* check that a ticket is conform to a set of assertion *)
let check_ticket (asserts:ticket_asserts) (t:bytes ticket) = 
    let (addr, (payload, total)),ticket = Tezos.read_ticket t in
    begin
    assert_with_errors_opt (fun (a,b:address*address) -> a=b)   asserts.addr addr "compare_tickets : wrong address";
    assert_with_errors_opt (fun (a,b:bytes*bytes) -> a=b)   asserts.payload payload "compare_tickets : wrong payload";
    assert_with_errors_opt (fun (a,b:nat*nat) -> a=b)   asserts.amount_  total"compare_tickets : wrong amount";
    ticket
    end

