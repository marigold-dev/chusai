type chusai_payload = bytes
type chusai = chusai_payload ticket
type chusai_ticket = chusai
type mint_parameter =
      Mint of chusai_ticket contract
    | Redeem of chusai_ticket * unit contract
type wallet_parameter =
    Store of chusai
    | Go_mint of address
    | Go_redeem of address
    | Nope