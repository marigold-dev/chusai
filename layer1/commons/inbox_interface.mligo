#import "ticket/chusai_ticket.mligo" "Ticket"
#import "transaction.mligo" "Tx"

type message =
  | Deposit of {owner: address; quantity: nat}
  | Transaction of Tx.transaction

type entrypoint =
  | Inbox_deposit of Ticket.t
  | Inbox_transaction of { destination : address; quantity : nat;  }
