#import "test_sc_receive.mligo" "Receive"
#import "test_sc_finalize.mligo" "Finalize_sc"
#import "test_chain.mligo" "Lib"
#import "test_finalize.mligo" "Finalize_lib"

let suites = [Receive.suite ; Finalize_sc.suite ; Finalize_lib.suite ; Lib.suite]