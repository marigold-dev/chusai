#include "stdlibext.mligo"

module TestExt =
  struct
    let originate 
        (type param storage) 
        (main : param * storage -> operation list * storage) 
        (initial_storage : storage) 
        (initial_tez : tez) : param contract * address = 
      let taddress, _, _ = Test.originate main initial_storage initial_tez in
      let contract = Test.to_contract taddress in 
      let contract_address = Tezos.address contract in
      (contract, contract_address)

    let is_success (result : test_exec_result) : bool = 
      match result with 
      Success _ -> true 
      | _ -> false

    let is_failure (result : test_exec_result) : bool = 
      not (is_success result)
      
    (* Returns a test_exec_result containing either the total gas of the execution or the first error *)
    let sum_gas (results : test_exec_result list) : test_exec_result =
      let rec go (results : test_exec_result list) (acc : nat) : test_exec_result =
        match results with 
          [] -> Success acc
          | (Fail e :: _) -> Fail e
          | (Success h :: t) -> go t (h + acc) in
      go results 0n

    let assert_equals (type a) (actual : a) (expected : a) : unit=
      if (Test.michelson_equal (Test.compile_value actual) (Test.compile_value expected))
      then
        unit
      else
        let _ = Test.log "******  Assertion failed. Values are not equal *********" in
        let _ = Test.log "Actual value:" in
        let _ = Test.log actual in
        let _ = Test.log "Expected value:" in
        let _ = Test.log expected in
        let _ = Test.log "********************************************************" in
        failwith "Assertion failed!"

    let assert_cond (type a) (actual : a) (predicate : a -> bool) : unit=
      if (predicate actual)
      then
        unit
      else
        let _ = Test.log "****** Assertion failed. The predicate returned 'false' *********" in
        let _ = Test.log "Actual value:" in
        let _ = Test.log actual in
        let _ = Test.log "*****************************************************************" in
        failwith "Assertion failed!"
  end

type metric = {
    name : string;
    body : unit -> test_exec_result list
  }

module Metric =
  struct
    let make (name : string) (body : unit -> test_exec_result list) : metric =
      { name = name;
        body = body
      }

    let run (metric : metric) : unit = 
      let results = metric.body () in
      let summed_result = TestExt.sum_gas results in
      let _ = Test.log "START METRIC" in
      let _ = match summed_result with 
        | Fail error -> 
          let _ = Test.log (StringExt.concat_all " : " [metric.name; "ERROR"]) in
          Test.log error
        | Success gas_used ->  
          let _ = Test.log (StringExt.concat_all " : " [metric.name; "SUCCESS"]) in
          Test.log gas_used in
      Test.log "END METRIC"
  end