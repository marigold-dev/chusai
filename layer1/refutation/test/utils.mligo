
#import "../src/game.mligo" "Game"
#import "../src/segment.mligo" "Seg"
#include "../../commons/refutation_interface.mligo"
let a = 0x0101
let b = 0x0202
let c = 0x0707
let d = 0x0808
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