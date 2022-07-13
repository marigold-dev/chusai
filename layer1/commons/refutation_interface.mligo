type hash = bytes

(** the length of a trace: nb of steps between two hashes*)
type size = nat

(** A segment represents part of a trace: 
it posits that a player can go from [h1] to [h2] in [size] moves.
See [segment.mligo] for API *)
type segment = hash * hash * size

(** A dissection is a series of two consecutive segments in a pair 
See [segment.mligo] for API *)
type dissection = segment * segment

(** When attacking a dissection, one has to indicate on which part of the dissection there is disagreement *)
type choice = Left | Right

(** players are represented by the address sending the moves *)
type player = address

(* States of game *)
(** The type of state of a game:
  - game was initialised, waiting for first move
  - last move was a dissection, provided by one of the players
  - the game has ended, player has to provide a proof for a segment
*)
type state = 
    | Start  of segment          
    | Dissection of player * dissection    
    | End    of player * segment 

(** Record storing the state of a game *)
type game = 
{   // players (A starts defending)
    player_a : player
;   player_b : player
    // current state
;   state    : state
}

(* ********************************** *)
(* Smart contract storage & parameters *)
(* The bissection smart contract is only provided for experimentation purposes *)

(* storage *)
type game_id = nat    

(** the map storing the games *)
type game_map = (game_id,game) big_map 

(** for each players, the list of games he's playing / has played *)
type player_map = (player,game_id list) big_map

type storage = 
{   max_id : game_id 
;   games  : game_map
;   games_of_players : player_map
}

(* parameter *)
(* There are two different endpoints for starting a game, corresponding to two possibles cases:
    - Endpoint_Start: any one can start a game between two competing blocks
    - Endpoint_Start_Dissection: someone can challenge a block by providing a bissection
   During a game, the players can send move using the [game_id]:
    - Endpoint_Dissection: a dissection of one of the segment in the current dissection (the [choice] is an [option] because at the beginning of the game there is only one segment that can be dissection)
    - Endpoint_Choose: designate one of the segment as final, in effect asking the other to provide a proof
 *)
type refutation_parameter =
    | Endpoint_Choose      of game_id * choice
    | Endpoint_Dissection       of game_id * dissection * (choice option)    
    | Endpoint_Start       of segment * player * player
    | Endpoint_Start_Dissection of segment * player * dissection *  player