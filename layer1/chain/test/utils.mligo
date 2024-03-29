#include "../src/chain.mligo"
#include "../test/chain_sc.mligo"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"

let bond : tez = 1000tez
let empty_chain : chain = 
    { max_index = 0n 
    ; blocks = (Big_map.empty : (index, block) big_map)
    ; children = (Big_map.empty : (index, index list) big_map)
    ; latest_finalized = 0n
    ; finality_period_in_days = 1n
    ; bond_amount = bond
    ; freezer = (Big_map.empty : fridge)
    }
type originated = Unit.originated // FIXME LIGO
type originated_chain = (chain_parameter, chain_storage) originated

let prototype_block : block = 
    {  index = 1n
    ;  parent = 0n
    ;  inbox_level = 0n
    ;  hash = 0x0101
    ;  proposer = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
    ;  date_of_proposition = ("2000-01-01t10:10:10Z" : timestamp)
    ;  cooler = (Map.empty : cooler)
    }

let block (index : index) (parent:index) (inbox_level:nat) : block = 
    {  prototype_block with
       index = index
    ;  parent = parent
    ;  inbox_level = inbox_level
    }
        
let prototype_block_proposal = 
    {  parent = 0n
    ;  inbox_level = 0n
    ;  hash = 0x0101
    ;  cooler = (Map.empty : cooler)
    }

let originate_chain () : originated_chain = 
    let empty_storage : chain_storage = 
        { empty_chain with
          max_index = 0n 
        ; blocks = (Big_map.empty : (index, block) big_map)
        ; children = (Big_map.empty : (index, index list) big_map)
        } in
    Unit.originate main empty_storage 0tez

let originate_chain_with (chain : chain) (bonds : tez) () : originated_chain =
    Unit.originate main chain bonds
    

let compare_proposal_and_block (proposal : block_proposal) (block_opt : block option) = 
    match block_opt with 
    | None -> false
    | Some my_block -> 
           proposal.parent = my_block.parent
        && proposal.inbox_level = my_block.inbox_level
        && proposal.hash = my_block.hash
