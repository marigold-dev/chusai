type gas = nat
type status
    = Status_Fail of string
    | Status_Fail_exec of test_exec_error
    | Status_Success of gas
    
let succeed () = Status_Success 0n
let fail (msg:string) = Status_Fail msg
let start = Status_Success 0n

let wrap_exec (result:test_exec_result) = 
    match result with
        | Success g -> Status_Success g
        | Fail error -> Status_Fail_exec error

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

let transfer_to_contract_ (type param) (contr : param contract) (action:param) (amount_:tez) =
    wrap_exec (Test.transfer_to_contract contr action amount_)

let transfer_to_contract (type param) (current:status) (contr : param contract) (action:param) (amount_:tez) =
    run current (fun () -> transfer_to_contract_ contr action amount_)

let assert_  (b:bool) (msg:string)  : status = 
    if b then succeed () else fail msg

let assert (current:status) (b:bool) (msg:string) : status = 
    // run current (fun () -> assert_  b msg) 
    and current (assert_ b msg)

let assert_is_ok (current:status) (msg:string) : status = 
    match current with
        | Status_Fail _ -> fail msg
        | Status_Fail_exec _ -> fail msg
        | Status_Success _ -> current


// let assert_rejected (current:status) (msg:string) : status =
//     match current with
//         | Status_Fail_exec (Rejected _) -> succeed ()
//         | _ -> fail msg

let assert_exec_error (current:status) (predicate:test_exec_error -> bool) (msg:string) = 
    match current with
        | Status_Fail_exec e -> assert_ (predicate e) msg
        | _ -> fail msg

let assert_rejected (current:status) (msg:string) : status =
    let predicate (e:test_exec_error) = match e with Rejected _ -> true | _ -> false in
    assert_exec_error current predicate msg

let assert_rejected_at (current:status) (addr:address) (msg:string) : status = 
    let predicate (e:test_exec_error) = match e with Rejected (_, contr_rejecting) -> addr = contr_rejecting |_ -> false in
    assert_exec_error current predicate msg


let equals_ (type a) (actual : a) (expected : a) : bool = 
    Test.michelson_equal (Test.compile_value actual) (Test.compile_value expected)

let assert_equals_ (type a) (actual : a) (expected : a)  (msg:string) : status =
  assert_ (equals_ actual expected) msg

let assert_equals (type a) (current:status)  (actual : a) (expected : a)  (msg:string) : status =
    // run current (fun () -> assert_equals_  actual expected msg) 
    and current (assert_equals_  actual expected msg)


let assert_cond_ (type a) (actual : a) (predicate : a -> bool)  (msg:string) : status =
    assert_ (predicate actual) msg

let assert_cond (type a) (current:status)  (actual : a) (predicate : a -> bool)  (msg:string) : status =
    // run current (fun () -> assert_cond_  actual predicate msg) 
    and current (assert_cond_  actual predicate msg)

let run_tests_ (type a) (combine:a*a -> a) (seed:a) (tests:(unit -> a) list)  =
    let results = List.map (fun (t:unit -> a) -> t ()) tests in
    begin
        Test.log results;
        List.fold_left combine seed results
    end

let run_tests_bool (tests:(unit -> bool) list) = run_tests_ 
    (fun (a,b:bool*bool) -> a && b) 
    true 
    tests

let run_tests (tests:(unit -> status) list) = run_tests_ 
    (fun (a,b:status*status) -> and a b) 
    (succeed ()) 
    tests