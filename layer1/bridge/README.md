# Inbox

The inbox is, for now, the entity which is responsible to receive deposit transactions requested by L1 users, and store them as messages that the validators on L2 will read.

The inbox is, for now, basically a state and a function for the deposit.

The state of the inbox contains 

1. the actual rollup level, which is an index for store the messages
2. the ticket which accumulates all the quantities of each deposit, for now, we support only one **key** (a key is the pair (address, payload), with address = the creator's ticket address).
3. the data structure which stores the messages.
4. the payload.

At each deposit requested, we join the ticket the user sent (if the key of this ticket is the same as the one fixed by the inbox state), create a message and, update the inbox state .