# A Simple Mint
- creates tickets with amount equal to number of mutez sent
- redeems ticket for mutez. Only tickets created by the mint can be redeemed, others fail tbe transaction.

# Callbacks
The `mint_sc` doesn't know anything about any `wallet_sc`. It uses callbacks to send the new ticket or to send back xtz.

# Parametrization
Some values, such as the payload of the tickets, are *parametrized* : they are provided in the storage defined at origination.

