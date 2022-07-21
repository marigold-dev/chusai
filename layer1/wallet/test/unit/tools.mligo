#include "../../../commons/wallet_interface.mligo"
#include "../../../stdlib_ext/src/stdlibext.mligo"

let dummy_address : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
let dummy_payload : bytes = 0x00

module Bridge = 
  struct
    let extract_ticket_from_storage (x : bridge_storage_human) : nat =
        OptionExt.default
          (Option.map
            (fun (ticket : Ticket.payload human_ticket) ->
              let {amount ; value = _ ; ticketer = _ } = ticket in
              amount
            )
            x.tickets)
          0n
  end


module Wallet =
  struct
    let extract_ticket_from_storage (x : wallet_storage_human) : nat =
        OptionExt.default
          (Option.map
            (fun (ticket : Ticket.payload human_ticket) ->
              let {amount ; value = _ ; ticketer = _ } = ticket in
              amount
            )
            x.ticket_storage)
          0n
  end