

type gas = nat

type failure_reason 
  = Message of string
  | Execution of test_exec_error

type status
  = Status_Fail of failure_reason
  | Status_Success of gas


  
(* Constructors *)

let succeed () = Status_Success 0n
let fail (msg:string) = Status_Fail (Message msg)
let start = Status_Success 0n

let is_failure (s:status):bool = 
    match s with 
    | Status_Success _ -> false
    | _ -> true

    
(* Combining/Chaining/Propagation of status *)

let and (s1:status) (s2:status) = 
    match s1,s2 with
    | Status_Success g1, Status_Success g2 -> Status_Success (g1 + g2)
    | Status_Success _, _ -> s2
    | _,_ -> s1
        
let and_list (l:status list) = 
    let and_ (s1, s2 : status * status) = and s1 s2 in
    List.fold_left and_ (succeed ()) l

let run (current:status)  (op: unit -> status)   =
    match current with
    | Status_Success _ -> and current (op ())
    | _ -> current