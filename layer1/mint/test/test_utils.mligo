
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
(* EXECUTION RESULT PROPAGATION *)
(*
    The LIGO Test module, defines type test_exec_result, to be returned by Test.transfer_to_contract, indicating if the operation failed or succeeded.
    Here are some functions taking inspiration from that, allowing to apply operations depending on the result of the previous one, and adding asserts.
    We don't define operators to make binding and such more readable, 'cause LIGO does not allow it 
    
    So to use it, do something like :
    let result = transfer_to_contract ... init_result in
    let result = assert_ ... result in
    let result = assert_ ... result in
    let result = assert_ ... result in
    result

    What's expected at the end is "Success n".

    Kinda ugly, but does the job as a first draft. Note that those functions are not pure, as transfer_to_contract has side effects.
*)
let init_result = Success 0n

(* wrap a message in a "Fail" statement *)
let fail (msg : string) = Fail (Other msg)

(* "transfer_to_contract" wraps Test.transfer_to_contract such that it is only applied if the previous operation resulted in Success *)
let transfer_to_contract (type a) (contr : a contract) (param : a) (amount_ : tez) (previous : test_exec_result) =
  match previous with
    | Success _ -> Test.transfer_to_contract contr param amount_ 
    | Fail _ -> previous 

(* "assert_" wraps an assertion in Fail or Success, or propagate previous Fail *)
let assert_ (b : bool) (msg : string) (previous : test_exec_result) = 
  match previous with
    | Fail _ -> 
        begin
        log_ ("assert_ : Fail was propagated ", previous);
        previous 
        end
    | Success _ -> if b then Success 0n else fail msg

let assert__ (b : bool) (msg : string) = assert_ b msg init_result


let is_none (type a) (opt : a option) =
    match opt with
        | None -> true
        | Some _t -> false

(* "assert_none_" wraps an assertion in Fail (not None) or Success (is None) , or propagate previous Fail *)
let assert_none_ (type a) (opt : a option) (msg : string) (previous : test_exec_result) = 
    assert_ (is_none opt) msg previous

(* "assert_rejected_at" check that the result is a Fail
    of type Rejected, emitted at a certain address, or propagates a Fail *)
let assert_rejected_at (at : address) (msg : string) (result : test_exec_result) =
    match result with
        | Success _ -> 
            begin
            log_ ("assert_rejected_at : previous action was not Rejected (but a Success)");
            fail msg
            end
        | Fail (Rejected (_, contr_rejecting)) -> 
            if at = contr_rejecting then Success 0n
            else
                begin 
                log_ (("assert_rejected_at : wrong address -> " ^ msg), result);
                fail ("assert_rejected_at : wrong address -> " ^ msg)
                end
        | _ -> 
            begin
            log_ ("assert_rejected_at : Fail was propagated ", result);
            result
            end

(* interrupt if last action was a fail, and replace fail message *)
let assert_is_ok (msg : string) (previous : test_exec_result) =
  match previous with
    | Fail _ -> 
        begin
        log_ ("assert_is_ok : was not ok, there was a fail ", previous);
        fail ("assert_is_ok : " ^ msg)
        end
    | Success _ -> previous


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
let check_ticket (asserts : ticket_asserts) (t : bytes ticket) = 
    let (addr, (payload, total)), ticket = Tezos.read_ticket t in
    begin
    assert_with_errors_opt (fun (a, b : address * address) -> a=b)   asserts.addr addr "compare_tickets : wrong address";
    assert_with_errors_opt (fun (a, b : bytes * bytes) -> a=b)   asserts.payload payload "compare_tickets : wrong payload";
    assert_with_errors_opt (fun (a, b : nat * nat) -> a=b)   asserts.amount_  total"compare_tickets : wrong amount";
    ticket
    end

