#import "ticket_api_workaround.mligo" "Ticket"

type message =
  | Deposit of {owner: address; quantity: nat}

type entrypoint =
  | Inbox_deposit of Ticket.chusai_ticket