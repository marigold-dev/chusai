#import "../stdlib_ext/src/atomic_test.mligo" "Atom"
#import "../mint/test/test_mint_sc.mligo" "Mint"
#import "../wallet/test/unit/test_mint_xtz.mligo" "Wallet_mint_xtz"
#import "../wallet/test/unit/test_redeem_xtz.mligo" "Wallet_redeem_xtz"
#import "../wallet/test/unit/test_send.mligo" "Wallet_send"

let test = Atom.run_suites [
    Mint.suite
  // ; Mint.suite2 
  ; Wallet_mint_xtz.suite
  ; Wallet_redeem_xtz.suite
  ; Wallet_send.suite
]