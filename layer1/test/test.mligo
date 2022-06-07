#import "../stdlib_ext/src/unit_test.mligo" "Unit"
#include "../stdlib_ext/src/stdlibext.mligo" 
#import "../mint/test/test_mint_sc.mligo" "Mint"
#import "../wallet/test/unit/suites.mligo" "Wallet"
#import "../stdlib_ext/test/suites.mligo" "Stdlib_test"
#import "../refutation/test/test_segment.mligo" "Refutation_segment"
#import "../refutation/test/test_game.mligo" "Refutation_game"

let test = 
  Unit.run_suites 
  ( ListExt.join
    [ Stdlib_test.suites
    ; Wallet.suites
    ; [ Mint.suite 
      ; Refutation_segment.suite
      ; Refutation_game.suite
      ]
    ]
  )
