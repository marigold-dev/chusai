#import "ticket/chusai_ticket.mligo" "Ticket"

type message =
  | Deposit of {owner: address; quantity: nat}

type entrypoint =
  | Inbox_deposit of Ticket.t