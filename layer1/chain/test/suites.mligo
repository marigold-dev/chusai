#import "test_sc_receive.mligo" "Receive"
#import "test_sc_finalize.mligo" "Finalize"
#import "test_chain.mligo" "Lib"

let suites = [Receive.suite ; Finalize.suite ; Lib.suite]