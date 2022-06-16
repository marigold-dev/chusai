#import "../stdlib_ext/src/unit_test.mligo" "Unit"
#include "../stdlib_ext/src/stdlibext.mligo" 
#import "../mint/test/test_mint_sc.mligo" "Mint"
#import "../wallet/test/unit/suites.mligo" "Wallet"
#import "../bridge/test/test_inbox_sc.mligo" "Inbox"
#import "../stdlib_ext/test/suites.mligo" "Stdlib_test"

let test = 
  Unit.run_suites 
  ( ListExt.join
    [ Stdlib_test.suites
    ; Wallet.suites
    ; [ Mint.suite ]
    ; [ Inbox.suite ]
    ]
  )
