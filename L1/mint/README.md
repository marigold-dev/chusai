# A Simple Mint
- create tickets with amount equal to nb of mutez sent
- redeem ticket for mutez

# Interface
```
type chusai = nat ticket
type mint_parameter =
      Mint 
    | Redeem of chusai
type wallet_parameter =
    Store of chusai
```