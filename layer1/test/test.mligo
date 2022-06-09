#import "../stdlib_ext/src/atomic_test.mligo" "Atom"
#include "../stdlib_ext/src/stdlibext.mligo" 
#import "../mint/test/test_mint_sc.mligo" "Mint"
#import "../wallet/test/unit/suites.mligo" "Wallet"
#import "../wallet/test/unit/test_redeem_xtz.mligo" "Wallet_redeem_xtz"
#import "../wallet/test/unit/test_send.mligo" "Wallet_send"
#import "../bridge/test/test_inbox_sc.mligo" "Inbox"
#import "../stdlib_ext/test/suites.mligo" "Stdlib_test"

let test = 
  Atom.run_suites 
  ( ListExt.join
    [ Stdlib_test.suites
    ; Wallet.suites
    ; [ Mint.suite ]
    ; [ Inbox.suite ]
    ]
  )
