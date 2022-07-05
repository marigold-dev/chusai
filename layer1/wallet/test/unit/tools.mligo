#include "../../../commons/wallet_interface.mligo"
#include "../../../stdlib_ext/src/stdlibext.mligo"

let dummy_address : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
let dummy_payload : bytes = 0x00

module Bridge = 
  struct
    let extract_ticket_from_storage (storage : bridge_storage) : nat = 
        OptionExt.default
          (Option.map
            (fun (ticket : Ticket.t) ->
              let content, _new_ticket = Ticket.read_ticket ticket in
              let _, (_, ticket_value) = content in
              ticket_value)
            storage.tickets)
          0n
  end


module Wallet =
  struct
    let extract_ticket_from_storage ({owner_address; mint_address; bridge_address; ticket_storage} : wallet_storage) : nat =
        OptionExt.default
          (Option.map
            (fun (ticket : Ticket.t) ->
              let content, _new_ticket = Ticket.read_ticket ticket in
              let _, (_, ticket_value) = content in
              ticket_value)
            ticket_storage)
          0n
  end