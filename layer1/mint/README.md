# A Simple Mint
- create tickets with amount equal to nb of mutez sent
- redeem ticket for mutez

# Interface
```
type chusai_payload = bytes
type chusai_ticket = chusai_payload ticket
type mint_parameter =
      Mint of chusai_ticket contract
    | Redeem of chusai_ticket * unit contract
```
