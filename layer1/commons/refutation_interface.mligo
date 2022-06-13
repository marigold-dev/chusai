type size = nat
type hash = nat
type segment = hash * hash * size
type split = segment * segment
type choice = Left | Right
type player = address

(* States of game *)
type state = 
    | Start  of segment
    | Split of player * split 
    | End    of player * segment

(* storage *)
type game_id = nat    
type game = {
    player_a : player;
    player_b : player;
    state    : state
}
type game_map = (game_id,game) big_map 
type storage = {
    max_id : game_id ;
    games  : game_map
}

(* parameter *)
type refutation_parameter =
    | Endpoint_Choose     of game_id * choice
    | Endpoint_Split      of game_id * split * (choice option)
    | Endpoint_Start of segment * player * player
    | Endpoint_Start_Split of segment * player * split *  player