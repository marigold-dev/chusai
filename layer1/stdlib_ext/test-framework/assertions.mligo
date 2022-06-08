
(* ASSERTIONS *)

let assert_  (b:bool) (msg:string)  : status = 
    if b then succeed () else fail msg

let assert_is_ok (current:status) (msg:string) : status = 
    match current with
    | Status_Fail _ -> fail msg
    | Status_Success _ -> current

let assert_exec_error (current:status) (predicate:test_exec_error -> bool) (msg:string) = 
    match current with
    | Status_Fail (Execution e) -> assert_ (predicate e) msg
    | _ -> fail msg

let assert_rejected (current:status) (msg:string) : status =
    let predicate (e:test_exec_error) = match e with Rejected _ -> true | _ -> false in
    assert_exec_error current predicate msg

let assert_rejected_at (current:status) (addr:address) (msg:string) : status = 
    let predicate (e:test_exec_error) = match e with Rejected (_, contr_rejecting) -> addr = contr_rejecting |_ -> false in
    assert_exec_error current predicate msg

let assert_rejected_with_error (current:status) (expected_error:michelson_program) (msg:string) = 
  let predicate (e:test_exec_error) = 
    match e with  
    | Rejected (error , _) -> error = expected_error
    | _ -> false
  in
  assert_exec_error current predicate msg

let equals_ (type a) (actual : a) (expected : a) : bool = 
    Test.michelson_equal (Test.compile_value actual) (Test.compile_value expected)

let assert_equals (type a) (actual : a) (expected : a)  (msg:string) : status =
  assert_ (equals_ actual expected) msg


let assert_cond (type a) (actual : a) (predicate : a -> bool)  (msg:string) : status =
    assert_ (predicate actual) msg
