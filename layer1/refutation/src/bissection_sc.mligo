(* MIT License

   Copyright (c) 2022 Marigold <contact@marigold.dev>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
   the Software, and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(* The Bissection smart contract *)
(* This smart contract is provided for experimentation purposes. 
    - It only exposes the library, and contain almost no business logic.
    - When the rest of the refutation game is implemented, this contract will have to expand, or be replaced.
 *)
#include "../../commons/refutation_interface.mligo"
#import "game.mligo" "Game"

(* UTILS *)
(** [find_game (game_id, storage)] helps finding a particular game inside the storage*)
let find_game (id, store : game_id * storage) =
    let opt = Big_map.find_opt id store.games in
    match opt with
    | None      -> (failwith "refutation_sc: no game at this id" : game)
    | Some game -> game

let add_game_for_player (player, game_id, player_map : player * game_id * player_map) : player_map =
    match Big_map.find_opt player player_map with 
    | None   -> Big_map.update player (Some [game_id]) player_map
    | Some l -> Big_map.update player (Some (game_id::l)) player_map

(** *)
let store_new_game (game, store : game * storage) : storage =
    let new_id = store.max_id + 1n in
    let new_games = Big_map.update new_id (Some game) store.games in
    let new_games_of_players = add_game_for_player ((game.player_a), new_id, store.games_of_players) in
    let new_games_of_players = add_game_for_player ((game.player_b), new_id, new_games_of_players) in
    {store with max_id = new_id; games = new_games; games_of_players = new_games_of_players}
    

(* ENDPOINTS *)
(** [update_game_in_store (id, game, storage)] updates the game in the [storage] at [id] with a new state [game] *)
let update_game_in_store (id,new_game,store : game_id * game * storage) = 
    {store with max_id = store.max_id; games = Big_map.update id (Some new_game) store.games} 

(** [start_game (segment, alice, bob, storage)] starts a game between [alice] (defending [segment]) and [bob]*)
let start_game (seg, player_a, player_b, store : segment * player * player * storage)  =
    let new_game = Game.Result.get_ok_or_raises (Game.start_game (seg, player_a ,player_b)) "refutation_sc: could not start game" in
    store_new_game (new_game, store)

(** [start_split_game (segment, alice, split, bob, storage)] starts a game between [alice] (defending [segment]) and [bob], 
    with [bob] starting his first move with [split] *)
let start_split_game (seg, player_a , split, player_b ,store : segment * player * split * player * storage)  =
    let new_game = Game.Result.get_ok_or_raises 
        (Game.start_split_game (Tezos.source, seg, player_a , split, player_b)) 
        "refutation_sc: could not start game" 
    in
    store_new_game (new_game, store)

(** [action_split (game_id, split, choice, storage)] apply [split] on the part [choice] of the current state of game [game_id]*)
let action_split (id, split, choice_opt, store : game_id * split * choice option * storage) : storage =
    let game = find_game (id, store) in
    let new_game = Game.Result.get_ok_or_raises (Game.apply_split (Tezos.source, split,choice_opt,game)) "refutation_sc: could not split" in
    update_game_in_store (id,new_game,store)

(** [action_choice (game_id, choice, storage)] choose the part [choice] on the current state of game [game_id] *)
let action_choice (id, choice, store : game_id * choice * storage) =
    let game = find_game (id, store) in
    let new_game = Game.Result.get_ok_or_raises (Game.apply_choice (Tezos.source, choice,game)) "refutation_sc: could not apply choice" in
    update_game_in_store (id,new_game,store)
  
(* MAIN *)

let main (action, store : refutation_parameter * storage) : operation list * storage = 
    match action with
    |  Endpoint_Choose (id, choice)                           -> [], action_choice (id, choice, store)
    |  Endpoint_Split (id, split, choice_opt)                 -> [], action_split (id, split, choice_opt, store)
    |  Endpoint_Start (seg, player_a ,player_b)               -> [], (start_game (seg, player_a ,player_b, store))
    |  Endpoint_Start_Split (seg, player_a , split, player_b) -> [], (start_split_game (seg, player_a , split, player_b, store))


(** Return the list of the current game I'm playing, if any *)
[@view] let my_games ((), store : unit * storage) : game_id list option = Big_map.find_opt Tezos.source store.games_of_players

(** Return the list of the current game I'm playing, return the game corresponding to an id *)
[@view] let get_game (game_id, store : game_id * storage) : game option = Big_map.find_opt game_id store.games