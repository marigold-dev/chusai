#import "ticket/chusai_ticket.mligo" "Ticket"
#import "transaction.mligo" "Tx"
#import "../chain/src/chain.mligo" "Chain"

type message =
  | Deposit of {owner: address; quantity: nat}
  | Freeze of {owner: address; quantity: nat}
  | Transaction of Tx.transaction

type entrypoint =
  | Inbox_deposit of Ticket.t
  | Inbox_transaction of { destination : address; quantity : nat;  }
  | Inbox_receive_block of Chain.block_proposal
  | Inbox_remove_block  of Chain.index // FIXME: delete when refutation is in place
  | Inbox_finalize_block
  | Inbox_freeze of {quantity: nat}
  | Inbox_withdraw
