#include "../../commons/refutation_interface.mligo"
#import "game.mligo" "Game"

(* UTILS *)

let find_game (id, store : game_id * storage) =
    let opt = Big_map.find_opt id store.games in
    match opt with
    | None      -> (failwith "no game at this id" : game)
    | Some game -> game

(* ENDPOINTS *)

let update_game_in_store (id,new_game,store : game_id * game * storage) = 
    let _ = if not (Game.check_player (Tezos.source, new_game)) then failwith "wrong player" in
    {max_id = store.max_id; games = Big_map.update id (Some new_game) store.games} 

let start_game (seg, player_a ,player_b ,store : segment * player * player * storage)  =
    let new_game = Game.make_game (seg, player_a ,player_b) in
    let new_id = store.max_id + 1n in
    // FIXME: need a way to communicate game id to players -> views ?
    {max_id = new_id; games = Big_map.update new_id (Some new_game) store.games} 


let action_split (id, split, choice, store : game_id * split * choice * storage) : storage =
    let game = find_game (id, store) in
    let new_game = Game.apply_split (split,choice,game) in
    update_game_in_store (id,new_game,store)

let action_choice (id, choice, store : game_id * choice * storage) =
    let game = find_game (id, store) in
    let new_game = Game.apply_choice (choice,game) in
    update_game_in_store (id,new_game,store)
  
(* MAIN *)

let main (_action, store : refutation_parameter * storage) : operation list * storage = 
    match _action with
    |  Choose (id, choice)                  -> [], action_choice (id, choice, store)
    |  Split (id, split, choice)                    -> [], action_split (id, split, choice, store)
    |  Start_game (seg, player_a ,player_b) -> [], (start_game (seg, player_a ,player_b, store))