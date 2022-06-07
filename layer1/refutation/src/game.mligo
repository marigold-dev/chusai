
#include "../../commons/refutation_interface.mligo"
#import "segment.mligo" "Seg"

let make_game (seg, player_a ,player_b : segment * player * player) =
    {  player_a = player_a
    ;  player_b = player_b
    ;  state    = Start seg
    }

(* LOGIC *)

let check_player (player, game : player * game) =
    let {player_a; player_b; state} = game in
    match state with
    | Start  _ -> player = player_b 
    | Bsplit _ -> player = player_a 
    | Asplit _ -> player = player_b 
    | _        -> true

let apply_split (split, choice, game : split * choice *  game) : game option = 
    let apply_split_aux (segment,make_game : segment * (split -> game)) =
         if not (Seg.check_split_against_segment (split, segment)) 
        then None
        else Some (make_game split) 
    in
    match game.state with
    | Start segment        -> 
        apply_split_aux ( segment, (fun (split:split) -> {game with state = Bsplit(split)}))

    | Bsplit old_split -> 
        let segment = Seg.choose (choice,old_split) in
        apply_split_aux ( segment, (fun (split:split) -> {game with state = Asplit(split)}))

    | Asplit old_split -> 
        let segment = Seg.choose (choice,old_split) in
        apply_split_aux ( segment, (fun (split:split) -> {game with state = Bsplit(split)}))
    | _ -> None
    
let apply_choice (choice, game : choice * game) : game option =
    match game.state with
    | Asplit old_split -> 
        let segment = Seg.choose (choice, old_split) in 
        let new_state = End (game.player_a,segment) in
        Some { game with state = new_state}

    | Bsplit old_split -> 
        let segment = Seg.choose (choice, old_split) in 
        let new_state = End (game.player_b,segment) in
        Some { game with state = new_state}

    | _ -> None