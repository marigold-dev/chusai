#import "../../stdlib_ext/src/stdlibext.mligo" "Stdlib"
#import "../../stdlib_ext/src/result.mligo" "Stdlib_Result"
type hash = bytes
type index = nat
type block_proposal = 
    {  parent : index
    ;  level : nat
    ;  hash : hash
    }
type block = 
    {  index : index
    ;  parent : index
    ;  level : nat
    ;  hash : hash
    ;  proposer : address
    ;  date_of_proposition : timestamp
    }
type chain = 
    {   max_index : index                         // max_index is the biggest index ever used. Not necessarily the biggest still used, as the corresponding block could have been removed.
    ;   blocks : (index, block) big_map        // stores the blocks
    ;   children : (index, index list) big_map  // stores the children of a block, i.e. the index of the blocks that designated it as immediat parent
    ;   latest_finalized : index
    ;   finality_period_in_days : nat
    ;   bond_amount : tez
    }

(* error type *) // FIXME: pretty minimal, if as no more value with finalization, relevance should be reexamined
type chain_error = 
    | Invalid_block
    | Could_not_find_details
    | No_candidate

let pp_chain_error (e : chain_error) : string =
    match e with
    | Invalid_block -> "Invalide block"
    | Could_not_find_details -> "Could not find the finalization candidate's details"
    | No_candidate -> "Could not find a finalization candidate"

type result = Stdlib_Result.t
    
let get_block (index, store : index * chain) : block option = Big_map.find_opt index store.blocks
let get_children (index, store : index * chain) : index list option = Big_map.find_opt index store.children

let make_block (proposal, index, proposer, now : block_proposal * index * address * timestamp) : block =
    {  index = index
    ;  parent = proposal.parent
    ;  level = proposal.level
    ;  hash = proposal.hash
    ;  proposer = proposer
    ;  date_of_proposition = now
    }

let increase_index (chain : chain) : chain =
    { chain with
      max_index = chain.max_index + 1n
    }

(* [is_block_valid (block, chain)] checks that a block is at least well formed *)
let is_block_valid (block, chain : block * chain) : bool = 
    block.parent = 0n || // takes care of the first block ever
    ( let parent_opt = get_block (block.parent, chain) in
    match parent_opt with
    | None -> false
    | Some parent -> parent.level < block.level )

(* [store_block (block,chain)] stores a block in the chain, checks validity, references it as a child. *)
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
        Ok ({ chain with 
              blocks = Big_map.update block.index (Some block) chain.blocks 
            ; children = add_to_children (block.index, block.parent, chain)
        })
    else Error Invalid_block

(* [remove_block (index,chain)] removes block at rank [index] from [chain]. Removes the children also. *)
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

(* FINALIZATION *)
let rec get_last_elt (type a) (l : a list) : a option =
    match l with
    | [] -> (None : a option)
    | t::[] -> Some t
    | t::q -> get_last_elt q 

let get_finalization_candidate_index (chain : chain) : index option =
    match get_children (chain.latest_finalized, chain) with
        | Some children -> get_last_elt children
        | None -> None

let get_finalization_candidate (chain : chain) : (block, chain_error) result = 
    match get_finalization_candidate_index chain with
    | None -> Error No_candidate
    | Some i -> 
        begin match get_block (i,chain) with
        | None -> Error Could_not_find_details
        | Some b -> Ok b
        end

let compute_finality_period (chain : chain) : int = 
    chain.finality_period_in_days * 84000

let check_age (date_of_birth, interval, date : timestamp * int * timestamp) : bool =
    date_of_birth + interval < date

let is_old_enough (block, chain, today : block * chain * timestamp) : bool =
   check_age (block.date_of_proposition, compute_finality_period chain, today)

let finalize (block, chain : block * chain) : chain =
    {chain with latest_finalized = block.index}
