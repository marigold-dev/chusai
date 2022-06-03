#include "../../commons/chusai_ticket_interface.mligo"
#include "../../commons/refutation_interface.mligo"

let find_game (id, store : game_id * storage) =
    let opt = Big_map.find_opt id store.games in
    match opt with
    | None      -> (failwith "no game at this id" : game)
    | Some game -> game

let make_game (seg, player_a ,player_b : segment * player * player) =
    {  player_a = player_a
    ;  player_b = player_b
    ;  state    = Start seg
    }

let start_game (seg, player_a ,player_b ,store : segment * player * player * storage)  =
    let new_game = make_game (seg, player_a ,player_b) in
    let new_id = store.max_id + 1n in
    // FIXME: need a way to communicate game id to players -> views ?
    {max_id = new_id; games = Big_map.update new_id (Some new_game) store.games} 

let check_player (player, game : player * game) =
    let {player_a; player_b; state} = game in
    match state with
    | Start  _ -> player <> player_b 
    | Bsplit _ -> player <> player_a 
    | Asplit _ -> player <> player_b 
    | _        -> true 

let apply_split (_split, game : split * game) : game = 
    // let {player_a; player_b; state} = game in
    match game.state with
    | Start  _seg       -> game
    | Bsplit _old_split -> game
    | Asplit _old_split -> game
    | _                -> (failwith "cannot apply split" : game)
    
let apply_choice (_choice, game : choice * game) : game =
    // let {player_a; player_b; state} = game in
    match game.state with
    | Asplit _old_split -> game
    | Bsplit _old_split -> game
    | _                -> (failwith "cannot apply split" : game)

(* ENDPOINTS *)

let update_game (id,new_game,store : game_id * game * storage) = 
    let _ = if not (check_player (Tezos.source, new_game)) then failwith "wrong player" in
    {max_id = store.max_id; games = Big_map.update id (Some new_game) store.games} 

let action_split (id, split, store : game_id * split * storage) : storage =
    let game = find_game (id, store) in
    let new_game = apply_split (split,game) in
    update_game (id,new_game,store)

let action_choice (id, choice, store : game_id * choice * storage) =
    let game = find_game (id, store) in
    let new_game = apply_choice (choice,game) in
    update_game (id,new_game,store)
  
(* MAIN *)
let main (_action, store : refutation_parameter * storage) : operation list * storage = 
    match _action with
    |  Choose (id, choice)                  -> [], action_choice (id, choice, store)
    |  Split (id, split)                    -> [], action_split (id, split, store)
    |  Start_game (seg, player_a ,player_b) -> [], (start_game (seg, player_a ,player_b, store))