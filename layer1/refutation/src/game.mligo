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

(* Game library *)
(* Define the transition of the bissection state machine:
   - allowed moves
   - how to start a game
   - how to apply moves

The transition functions return a game if they succeeded, or None if the move was not legal.
*)

#include "../../commons/refutation_interface.mligo"
#import "segment.mligo" "Seg"

(** [check_player (alice, game)] check that it's [alice]'s turn in [game] *)
let check_player (proposer, game : player * game) =
    let {player_a; player_b; state} = game in
    match state with
    | Start  _ -> proposer = player_b 
    | Split (last_player,_) -> proposer <> last_player &&  (proposer = game.player_a ||proposer = player_b)
    | _        -> true

(** [start_game (segment, alice, bob)] starts a game with [alice] defending [segment] against [bob] *)
let start_game (seg, player_a ,player_b : segment * player * player) : game option =
    if (Seg.size seg) = 1n then None
    else
        Some 
            {  player_a = player_a
            ;  player_b = player_b 
            ;  state    = Start seg
            }

(** [apply_split (proposer, split, choice, game)] try a play by [proposer]: 
    Split of value [split] of the part [choice] of the current split of [game] *)
let apply_split (proposer, split, choice_opt, game : player * split * (choice option) * game) : game option = 
    if not (check_player (proposer, game)) then None 
    else
        let apply_if_possible (segment : segment ) : game option =
            if Seg.check_split_against_segment (split, segment) 
            then Some {game with state = Split(proposer,split)}
            else None
        in
        match game.state, choice_opt with
        | Start segment, None              -> apply_if_possible segment
        | Split (_,old_split), Some choice -> apply_if_possible (Seg.choose (choice,old_split))
        | _ -> None

(** [start_game (proposer, segment, alice, split, bob)] starts a game with [alice] defending [segment] against [bob], whose first move is [split]. 
    The game was started by [proposer]. *)
let start_split_game (proposer, seg, player_a , split, player_b : player * segment * player * split * player) : game option =
    if proposer <> player_b then None
        else 
        let game : game option = start_game (seg, player_a , player_b) in
        match game with 
        | None -> None 
        | Some g -> apply_split (player_b, split, (None : choice option), g)

(** [apply_choice (proposer, choice, game)] the player [proposer] make a [choice] among the parts of the last split in [game]*)
let apply_choice (proposer, choice, game : player * choice * game) : game option =
    if not (check_player (proposer, game)) then None        
    else
        match game.state with
        | Split (last_player,old_split) -> 
            let segment = Seg.choose (choice, old_split) in 
            if Seg.size segment > 1n then None
            else Some { game with state = End (last_player,segment)}

        | _ -> None