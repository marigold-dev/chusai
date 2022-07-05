
#import "../src/game.mligo" "Game"
#import "../src/segment.mligo" "Seg"
#import "../src/bissection_sc.mligo" "Bissection"
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

    
(* UTILS *)
// FIXME: (LIGO) need to redefine the type to allow specialization alias definition
type originated = Unit.originated

// define an alias for the specialization of type [originated]
type originated_arbiter = (Bissection.refutation_parameter, Bissection.storage) originated
let bissection_default_storage = 
    {  max_id = 0n
    ;  games = (Big_map.empty : Bissection.game_map)
    ;  games_of_players = (Big_map.empty : Bissection.player_map)
    }

(** originate the bissection game contract *)
let originate_bissection () = 
    Unit.originate Bissection.main bissection_default_storage 0tez

(** find the [max_id] in the storage*)
let get_max_id (arbiter : originated_arbiter) = 
    let storage = Test.get_storage arbiter.originated_typed_address in
    storage.max_id

(** find a game in the storage *)
let get_game (arbiter : originated_arbiter) (id : game_id) = 
    let storage = Test.get_storage arbiter.originated_typed_address in
    Big_map.find_opt id storage.games

let originate_bissection_from_file () = 
    let file = "refutation/src/bissection_sc.mligo" in
    let views = ["get_game"; "my_games"] in
    let storage = Test.compile_value bissection_default_storage in
    let address, _, _ = Test.originate_from_file file "main" views storage 0tez in
    let typed_address : (Bissection.refutation_parameter, Bissection.storage) typed_address = Test.cast_address address in    
    let contract = Test.to_contract typed_address in
    { originated_typed_address = typed_address
    ; originated_contract = contract
    ; originated_address = address }
