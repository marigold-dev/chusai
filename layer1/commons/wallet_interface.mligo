#import "ticket/tezos_ticket.mligo" "Ticket"

type chusai_ticket_storage = Ticket.t option

type 'a wallet_storage_ = {
  owner_address : address;
  mint_address : address;
  bridge_address : address;
  ticket_storage : 'a
}

type wallet_storage = chusai_ticket_storage wallet_storage_

type wallet_storage_human = (Ticket.payload human_ticket) option wallet_storage_

type wallet_parameter 
  = Mint_xtz
  | Mint_xtz_cb of Ticket.t
  | Send 
  | Redeem_xtz 
  | Redeem_xtz_cb 

type wallet_return = operation list * wallet_storage

type bridge_storage = {
  tickets : chusai_ticket_storage
}

type bridge_parameter = Deposit of Ticket.t

type bridge_return = operation list * bridge_storage