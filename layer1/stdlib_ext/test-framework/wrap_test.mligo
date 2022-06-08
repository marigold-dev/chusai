
(* Wrapping of Test module *)

let wrap_exec (result:test_exec_result) = 
    match result with
    | Success g -> Status_Success g
    | Fail error -> Status_Fail ( Execution error)

let transfer_to_contract_ (type param) (contr : param contract) (action:param) (amount_:tez) =
    wrap_exec (Test.transfer_to_contract contr action amount_)

let transfer_to_contract (type param) (current:status) (contr : param contract) (action:param) (amount_:tez) =
    run current (fun () -> transfer_to_contract_ contr action amount_)