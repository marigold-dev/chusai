#import "test_sc_receive.mligo" "Receive_sc"
#import "test_sc_finalize.mligo" "Finalize_sc"
#import "test_chain.mligo" "Lib"
#import "test_finalize.mligo" "Finalize_lib"
#import "test_withdraw.mligo" "Withdraw_lib"
#import "test_sc_withdraw.mligo" "Withdraw_sc"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
let suites = [  Receive_sc.suite ; Finalize_sc.suite ; Finalize_lib.suite ; Lib.suite ; Withdraw_lib.suite ; Withdraw_sc.suite ]
