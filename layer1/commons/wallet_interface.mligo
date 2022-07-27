#import "ticket/chusai_ticket.mligo" "Ticket"
#import "../chain/src/chain_endpoints.mligo" "Chain"

type chusai_ticket_storage = Ticket.t option

type wallet_storage = [@layout:comb] {
  owner_address : address;
  mint_address : address;
  bridge_address : address;
  ticket_storage : chusai_ticket_storage
}

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

type bridge_parameter
  = Deposit of Ticket.t
  | Transaction of { destination : address; quantity : nat;  }
  | Receive_block of Chain.block_proposal
  | Remove_block  of Chain.index // FIXME: delete when refutation is in place
  | Finalize_block
  | Freeze of {quantity: nat}
  | Withdraw of Ticket.t contract


type bridge_return = operation list * bridge_storage
