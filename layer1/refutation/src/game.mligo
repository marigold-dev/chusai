
#include "../../commons/refutation_interface.mligo"
#import "segment.mligo" "Seg"

let check_player (proposer, game : player * game) =
    let {player_a; player_b; state} = game in
    match state with
    | Start  _ -> proposer = player_b 
    | Split (last_player,_) -> proposer <> last_player &&  (proposer = game.player_a ||proposer = player_b)
    | _        -> true

let start_game (seg, player_a ,player_b : segment * player * player) : game option =
    if (Seg.size seg) = 1n then None
    else
        Some 
            {  player_a = player_a
            ;  player_b = player_b 
            ;  state    = Start seg
            }

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

let start_split_game (proposer, seg, player_a , split, player_b : player * segment * player * split * player) : game option =
    if proposer <> player_b then None
        else 
        let game : game option = start_game (seg, player_a , player_b) in
        match game with 
        | None -> None 
        | Some g -> apply_split (player_b, split, (None : choice option), g)

let apply_choice (proposer, choice, game : player * choice * game) : game option =
    if not (check_player (proposer, game)) then None        
    else
        match game.state with
        | Split (last_player,old_split) -> 
            let segment = Seg.choose (choice, old_split) in 
            if Seg.size segment > 1n then None
            else Some { game with state = End (last_player,segment)}

        | _ -> None