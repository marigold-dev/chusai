#import "../stdlib_ext/src/atomic_test.mligo" "Atom"
#include "../stdlib_ext/src/stdlibext.mligo" 
#import "../mint/test/test_mint_sc.mligo" "Mint"
#import "../wallet/test/unit/test_mint_xtz.mligo" "Wallet_mint_xtz"
#import "../wallet/test/unit/test_redeem_xtz.mligo" "Wallet_redeem_xtz"
#import "../wallet/test/unit/test_send.mligo" "Wallet_send"
#import "../stdlib_ext/test/suites.mligo" "Stdlib_test"

let test = 
  Atom.run_suites 
  ( ListExt.concat_all
    [ Stdlib_test.suites
    ; [ Mint.suite
      ; Wallet_mint_xtz.suite
      ; Wallet_redeem_xtz.suite
      ; Wallet_send.suite
      ]
    ]
  )
