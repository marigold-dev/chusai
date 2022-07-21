#import "../../stdlib_ext/src/stdlibext.mligo" "Stdlib"
#import "../../stdlib_ext/src/result.mligo" "Stdlib_Result"
type hash = bytes
type index = nat
type block = 
    {  parent : index
    ;  level : nat
    ;  hash : hash
    ;  proposer : address
    }
type chain = 
    {   max_index : index                         // max_index is the biggest index ever used. Not necessarily the biggest still used, as the corresponding block could have been removed.
    ;   blocks : (index, block) big_map        // stores the blocks
    ;   children : (index, index list) big_map  // stores the children of a block, i.e. the index of the blocks that designated it as immediat parent
    }

(* error type *) // FIXME: pretty minimal, if as no more value with finalization, relevance should be reexamined
type chain_error = 
    | Invalid_block
type result = Stdlib_Result.t
    
let get_block (index, store : index * chain) : block option = Big_map.find_opt index store.blocks
let get_children (index, store : index * chain) : index list option = Big_map.find_opt index store.children

(* [is_block_valid (block, chain)] checks that a block is at least well formed *)
let is_block_valid (block, chain : block * chain) : bool = 
    block.parent = 0n || // takes care of the first block ever
    ( let parent_opt = get_block (block.parent, chain) in
    match parent_opt with
    | None -> false
    | Some parent -> parent.level < block.level )

(* [store_block (block,chain)] stores a block in the chain, checking validity, referencing it as a children. *)
let store_block (block, chain : block * chain) : (chain, chain_error) result =
    let add_to_children (newborn, parent, chain : index * index * chain) : (index, index list)  big_map  =
        let new_children = 
            match get_children (parent, chain) with
            | None ->  [newborn]
            | Some siblings -> newborn :: siblings
        in
        Big_map.update parent (Some new_children) chain.children
    in
    if is_block_valid (block,chain) then
        let new_index = chain.max_index + 1n in
        Ok ({ chain with 
              blocks = Big_map.update new_index (Some block) chain.blocks 
            ; max_index =  new_index
            ; children = add_to_children (new_index, block.parent, chain)
        })
    else Error Invalid_block

(* [remove_block (index,chain)] remove block at rank [index] from [chain]. Removes the children also. *)
let remove_block (index, chain: index * chain) : chain =
    let delete_block (chain, index: chain * index) : chain =
        {chain with blocks = Big_map.update index (None : block option) chain.blocks} 
    in
    let rec aux (to_delete, chain : index list * chain) : chain = 
        match to_delete with
        | [] -> chain
        | i::q -> 
            begin
            let children = Big_map.find_opt i chain.children in
            let chain = delete_block (chain, i) in
            match children with
            | None -> aux (q, chain)
            | Some children -> aux (Stdlib.ListExt.concat children q, chain)
            end
    in
    aux ([index],chain)

(* [find_latest_existing (chain)] finds the block of biggest index, that is still present in chain. *)
let find_latest_existing (chain : chain) : block option = 
    let rec find_existing (index : index) : block option =
        if index = 0n then (None : block option)
        else match Big_map.find_opt index chain.blocks with
            | None -> find_existing (abs(index - 1n))
            | Some block -> Some block
    in
    let max_index = chain.max_index in
    find_existing max_index

