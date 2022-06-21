#import "ticket/chusai_ticket.mligo" "Ticket"

type mint_parameter =
      Mint of Ticket.t contract
    | Redeem of Ticket.t * unit contract