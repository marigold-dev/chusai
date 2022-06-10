
#import "../src/game.mligo" "Game"
#import "../src/segment.mligo" "Seg"
let a = 0n
let b = 1n
let c = 3n
let d = 4n
let ac (size:size) = Seg.make_segment a c size
let ab (size:size) = Seg.make_segment a b size
let ad (size:size) = Seg.make_segment a d size
let bc (size:size) = Seg.make_segment b c size
let bd (size:size) = Seg.make_segment b d size
let cd (size:size) = Seg.make_segment c d size

let player1 () = Test.nth_bootstrap_account 0 
let player2 () = Test.nth_bootstrap_account 1
let player3 () = Test.nth_bootstrap_account 2 

let init_game () = 
    let _ = Test.reset_state 5n ([]:tez list) in
    {  player_a = player1 ()
    ;  player_b = player2 ()
    ;  state    = Start (ac 5n)
    }

let init_game_ (new_state:state) = 
    let game = init_game () in 
    {game with state=new_state}