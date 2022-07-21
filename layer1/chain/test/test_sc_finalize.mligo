#include "../src/chain_sc.mligo"
#include "utils.mligo"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"

let _tbi () = Unit.fail_with "TBI"
(* UTILS *)
type originated = Unit.originated // FIXME LIGO
type originated_chain = (chain_parameter, chain_storage) originated
let bond : tez = 1tez
let empty_chain = 
    { max_index = 0n 
    ; blocks = (Big_map.empty : (index, block) big_map)
    ; children = (Big_map.empty : (index, index list) big_map)
    ; latest_finalized = 0n
    ; finality_period_in_days = 7n
    ; bond_amount = bond
    }

let prototype_block = 
        {  parent = 0n
        ;  level = 0n
        ;  hash = 0x0101
        ;  proposer = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
        ;  date_of_proposition = ("2000-01-01t10:10:10Z" : timestamp)
        }

let originate_chain () : originated_chain = 
    let empty_storage : chain_storage = 
        { empty_chain with
          max_index = 0n 
        ; blocks = (Big_map.empty : (index, block) big_map)
        ; children = (Big_map.empty : (index, index list) big_map)
        } in
    Unit.originate main empty_storage 0tez

(* TESTS *)


// test sending a block
let _test_finalize_first () = _tbi ()
let _test_finalize_second = _tbi
let _test_finalize_old = _tbi
let _test_finalize_orphan = _tbi

(* Creation of test suite *)
let suite = Unit.make_suite
"Chain_sc : finalize"
"Test suite of finalisation of blocks"
[  Unit.make_test "finalize first block" "test finalizing the first block ever"  _test_finalize_first          
;  Unit.make_test "finalize second block" "test finalizing a second block" _test_finalize_second            
;  Unit.make_test "finalize second block (there some after)" "test finalizing a second block, when there are more waiting" _test_finalize_old                   
;  Unit.make_test "finalize block with no proposer" "test finalizing a block with no correct proposer" _test_finalize_orphan           
]