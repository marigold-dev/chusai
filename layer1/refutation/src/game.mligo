
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
    | Start  _ -> player <> player_b 
    | Bsplit _ -> player <> player_a 
    | Asplit _ -> player <> player_b 
    | _        -> true 

let apply_split (split, choice, game : split * choice *  game) : game = 
    match game.state with
    | Start segment        ->
        // FIXME no use for 'choice' here, should be an option, checked against None ?
        let _ = if not (Seg.check_split_against_segment (split, segment)) then failwith "Split incompatible with segment" in
        {game with state = Bsplit(split)}

    | Bsplit old_split -> 
        let segment = Seg.choose (choice,old_split) in
        let _ = if not (Seg.check_split_against_segment (split, segment)) then failwith "Split incompatible with segment" in
        {game with state = Asplit(split)}

    | Asplit old_split -> 
        let segment = Seg.choose (choice,old_split) in
        let _ = if not (Seg.check_split_against_segment (split, segment)) then failwith "Split incompatible with segment" in
        {game with state = Bsplit(split)}

    | _                 -> (failwith "cannot apply split" : game)
    
let apply_choice (_choice, game : choice * game) : game =
    match game.state with
    | Asplit _old_split -> game
    | Bsplit _old_split -> game
    | _                -> (failwith "cannot apply choice" : game)