#include "ticket_api_workaround.mligo"

type mint_parameter =
      Mint of chusai_ticket contract
    | Redeem of chusai_ticket * unit contract