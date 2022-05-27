type chusai_ticket_storage = chusai_ticket option

type wallet_storage = {
  mint_address : address;
  bridge_address : address;
  ticket_storage : chusai_ticket_storage
}

type wallet_parameter 
  = Mint_xtz
  | Mint_xtz_cb of chusai_ticket
  | Send 
  | Redeem_xtz 
  | Redeem_xtz_cb 

type wallet_return = operation list * wallet_storage

type bridge_storage = {
  tickets : chusai_ticket_storage
}

type bridge_parameter = Deposit of chusai_ticket

type bridge_return = operation list * bridge_storage