#include "../src/chain.mligo"
#include "../src/chain_sc.mligo"
let block (index : index) (parent:index) (level:nat) : block = 
    {  index = index
    ;  parent = parent
    ;  level = level
    ;  hash = 0x0101
    ;  proposer = ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)
    ;  date_of_proposition = ("2000-01-01t10:10:10Z" : timestamp)
    }
        
let empty_chain : chain = 
    { max_index = 0n 
    ; blocks = (Big_map.empty : (index, block) big_map)
    ; children = (Big_map.empty : (index, index list) big_map)
    ; latest_finalized = 0n
    ; finality_period_in_days = 7n
    ; bond_amount = 1tez
    }
let bond : tez = 1tez
type originated = Unit.originated // FIXME LIGO
type originated_chain = (chain_parameter, chain_storage) originated

let prototype_block = 
    {  parent = 0n
    ;  level = 0n
    ;  hash = 0x0101
    ;  proposer = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
    ;  date_of_proposition = ("2000-01-01t10:10:10Z" : timestamp)
    }

let prototype_block_proposal = 
    {  parent = 0n
    ;  level = 0n
    ;  hash = 0x0101
    }

let originate_chain () : originated_chain = 
    let empty_storage : chain_storage = 
        { empty_chain with
          max_index = 0n 
        ; blocks = (Big_map.empty : (index, block) big_map)
        ; children = (Big_map.empty : (index, index list) big_map)
        } in
    Unit.originate main empty_storage 0tez