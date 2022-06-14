#import "test_mint_xtz.mligo" "Wallet_mint_xtz"
#import "test_redeem_xtz.mligo" "Wallet_redeem_xtz"
#import "test_send.mligo" "Wallet_send"
#import "test_ownership.mligo" "Wallet_owner"

let suites = 
    [ Wallet_mint_xtz.suite
    ; Wallet_redeem_xtz.suite
    ; Wallet_send.suite
    ; Wallet_owner.suite
    ]