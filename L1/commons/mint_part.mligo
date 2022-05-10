type chusai_payload = bytes
type chusai = chusai_payload ticket
type mint_parameter =
      Mint 
    | Redeem of chusai
type wallet_parameter =
    Store of chusai
    | Go_mint of address
    | Go_redeem of address
    | Nope