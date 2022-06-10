#include "../../commons/refutation_interface.mligo"
#import "game.mligo" "Game"

(* UTILS *)

let find_game (id, store : game_id * storage) =
    let opt = Big_map.find_opt id store.games in
    match opt with
    | None      -> (failwith "refutation_sc: no game at this id" : game)
    | Some game -> game

(* ENDPOINTS *)

let update_game_in_store (id,new_game,store : game_id * game * storage) = 
    // FIXME: should be checked by Game library 
    let _ = if not (Game.check_player (Tezos.source, new_game)) then failwith "refutation_sc: wrong player" in
    {max_id = store.max_id; games = Big_map.update id (Some new_game) store.games} 

let start_game (seg, player_a ,player_b ,store : segment * player * player * storage)  =
    let new_game = Option.unopt_with_error (Game.start_game (Tezos.source, seg, player_a ,player_b)) "refutation_sc: could not start game" in
    let new_id = store.max_id + 1n in
    // FIXME: need a way to communicate game id to players -> views ?
    {max_id = new_id; games = Big_map.update new_id (Some new_game) store.games} 


let action_split (id, split, choice, store : game_id * split * choice * storage) : storage =
    let game = find_game (id, store) in
    let new_game = Option.unopt_with_error (Game.apply_split (Tezos.source, split,choice,game)) "refutation_sc: could not split" in
    update_game_in_store (id,new_game,store)

let action_choice (id, choice, store : game_id * choice * storage) =
    let game = find_game (id, store) in
    let new_game = Option.unopt_with_error (Game.apply_choice (Tezos.source, choice,game)) "refutation_sc: could not apply choice" in
    update_game_in_store (id,new_game,store)
  
(* MAIN *)

let main (_action, store : refutation_parameter * storage) : operation list * storage = 
    match _action with
    |  Endpoint_Choose (id, choice)             -> [], action_choice (id, choice, store)
    |  Endpoint_Split (id, split, choice_opt)       -> [], action_split (id, split, choice_opt, store)
    |  Endpoint_Start (seg, player_a ,player_b) -> [], (start_game (seg, player_a ,player_b, store))