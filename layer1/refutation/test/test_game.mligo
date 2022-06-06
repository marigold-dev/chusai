#include "../../commons/refutation_interface.mligo"
#import "../src/game.mligo" "Game"
#import "../../stdlib_ext/src/atomic_test.mligo" "Atom"

let suite = Atom.make_suite 
    ""
    [ Atom.make_test "" "" (fun () -> (Status_Success 0n))

    ]