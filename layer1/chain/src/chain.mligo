#import "../../stdlib_ext/src/stdlibext.mligo" "Stdlib"
#import "../../stdlib_ext/src/result.mligo" "Stdlib_Result"
type hash = bytes
type index = nat
(** The information sent by a user proposing a block *)
type block_proposal = 
    {  parent : index
    ;  level : nat
    ;  hash : hash
    }
(** A complete block: proposal + metadata *)
type block = 
    {  index : index
    ;  parent : index
    ;  level : nat
    ;  hash : hash
    ;  proposer : address
    ;  date_of_proposition : timestamp
    }
(** Storage of blocks, informations on finalized blocks, metadata of a chain (bond, finality period) *)
type chain = 
    {   max_index : index                         // max_index is the biggest index ever used. Not necessarily the biggest still used, as the corresponding block could have been removed.
    ;   blocks : (index, block) big_map           // stores the blocks
    ;   children : (index, index list) big_map    // stores the children of a block, i.e. the index of the blocks that designated it as immediat parent
    ;   latest_finalized : index                  // index of latest finalized block
    ;   finality_period_in_days : nat             // finality period, in days
    ;   bond_amount : tez                         // amount asked for bond, to be placed when proposing a block
    }

(* error type *)
type chain_error = 
    | Invalid_block
    | Could_not_find_details
    | No_candidate

let pp_chain_error (e : chain_error) : string =
    match e with
    | Invalid_block -> "Invalide block"
    | Could_not_find_details -> "Could not find the finalization candidate's details"
    | No_candidate -> "Could not find a finalization candidate"

// FIXME: LIGO 
type result = Stdlib_Result.t
    
(** [get_block (i,chain)] finds a particular block by its index *)
let get_block (index, chain : index * chain) : block option = Big_map.find_opt index chain.blocks

(** [get_children (i,chain)] finds the children of a block: the list of blocks that declared [i] as their immediat parent *)
let get_children (index, chain : index * chain) : index list option = Big_map.find_opt index chain.children

(** [make_block (proposal, i, proposer, now)] creates a block from [proposal], including metadata corresponding to the [proposer], using index [i], and registering date_of_proposition [now] *)
let make_block (proposal, index, proposer, now : block_proposal * index * address * timestamp) : block =
    {  index = index
    ;  parent = proposal.parent
    ;  level = proposal.level
    ;  hash = proposal.hash
    ;  proposer = proposer
    ;  date_of_proposition = now
    }

(** [increase_index (chain)] returns a new chain with the max_index increased *)
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
let remove_block (index, chain : index * chain) : block list * chain =
    let delete_block (chain, index : chain * index) : block option * chain =
        // FIXME: LIGO: should be get_and_update but it does work in test (Ubound primitive.)
        // let block, new_blocks = Big_map.get_and_update index (None : block option) chain.blocks in
        let block = Big_map.find_opt index chain.blocks in
        let new_blocks = Big_map.update index (None : block option) chain.blocks in
        block, {chain with blocks = new_blocks} 
    in
    let rec aux (deleted, to_delete, chain : block list * index list * chain) : block list *chain = 
        match to_delete with
        | [] -> deleted, chain
        | i::q -> 
            begin
            let block, chain = delete_block (chain, i) in
            let new_deleted = 
                match block with
                | None -> deleted
                | Some b -> b::deleted
            in
            let children = Big_map.find_opt i chain.children in            
            match children with
            | None -> aux (new_deleted, q, chain)
            | Some children -> aux (new_deleted, Stdlib.ListExt.concat children q, chain)
            end
    in
    aux (([] : block list), [index], chain)

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
(** auxialiary function: [get_last_elt (a::b::c::...::[z])] returns the last element in the list: (Some z) *)
let rec get_last_elt (type a) (l : a list) : a option =
    match l with
    | [] -> (None : a option)
    | t::[] -> Some t
    | t::q -> get_last_elt q 

(** [get_finalization_candidate_index (chain)] returns the index of a block, which is the first registered child of last finalized block *)
let get_finalization_candidate_index (chain : chain) : index option =
    match get_children (chain.latest_finalized, chain) with
        | Some children -> get_last_elt children
        | None -> None

(** [get_finalization_candidate (chain)] returns the next block to finalize *)
let get_finalization_candidate (chain : chain) : (block, chain_error) result = 
    match get_finalization_candidate_index chain with
    | None -> Error No_candidate
    | Some i -> 
        begin match get_block (i,chain) with
        | None -> Error Could_not_find_details
        | Some b -> Ok b
        end


(* FINALITY PERIOD CALCULATION *)
let compute_finality_period_seconds (chain : chain) : int = 
    chain.finality_period_in_days * 84000

(** [check_age (dob, interval, date)] checks that [dob] is at least [interval] before [date] *)
let check_age (date_of_birth, interval, date : timestamp * int * timestamp) : bool =
    date_of_birth + interval < date

(** [is_old_enough (block, chain, date)] checks that the finalirt period of a [block] is finished, with respect to a [chain], at a certain [date] *)
let is_old_enough (block, chain, today : block * chain * timestamp) : bool =
   check_age (block.date_of_proposition, compute_finality_period_seconds chain, today)

(** [finalize (block, chain)] registers in [chain] that [block] is finalized *)
let finalize (block, chain : block * chain) : chain =
    {chain with latest_finalized = block.index}
