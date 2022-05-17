type chusai_payload = bytes
type chusai_ticket = chusai_payload ticket
type mint_parameter =
      Mint of chusai_ticket contract
    | Redeem of chusai_ticket * unit contract

