(** Aliases for allowing switching between real and dummy implementation. *)
type payload = bytes

(** Characterize a [Dummy_ticket]. *)
type t =
  Dummy_ticket of {
    address: address
  ; payload: payload
  ; quantity: nat
  }

(** When a ticket has been read. *)
type ticket_content = (address * (payload * nat)) * t

(** When a ticket has beed split. *)
type ticket_split = (t * t) option

(** [Dummy_ticket.create_ticket payload qty]
    Create a [Dummy_ticket]. *)
let create_ticket (address: address) (payload: bytes) (quantity: nat) =
  Dummy_ticket {
    address = address
  ; payload = payload
  ; quantity = quantity
  }

(** [Dummy_ticket.read_ticket a_dummy_ticket]
    Read a [Dummy_ticket]. *)
let read_ticket (ticket: t) : ticket_content =
  match ticket with
  | Dummy_ticket { address = address; payload = payload; quantity = value } ->
    ((address, (payload, value)), ticket)


(** [Dummy_ticket.join_tickets left_ticket right_ticket]
    Join two [Dummy_ticket] of the same kind. *)
let join_tickets (left: t) (right: t) : t option =
  match (left, right) with
  | Dummy_ticket { address = address_a; payload = payload_a; quantity = value_a },
    Dummy_ticket { address = address_b; payload = payload_b; quantity = value_b } ->
      if payload_a = payload_b && address_a = address_b
      then
        let joined_ticket = Dummy_ticket {
          address = address_a
        ; payload = payload_a
        ; quantity = value_a + value_b
        } in Some joined_ticket
      else None


(** [Dummy_ticket.split ticket left_part right_part]
    If [left_part + right_part] = ticket.quantity, it produces two
    new tickets. *)
let split_ticket (ticket: t) (left_amount: nat) (right_amount: nat) : ticket_split =
  match ticket with
  | Dummy_ticket { address = address; payload = payload; quantity = value } ->
    if value = left_amount + right_amount
    then
      let left_ticket = Dummy_ticket {
        address = address
      ; payload = payload
      ; quantity = left_amount }
      in
      let right_ticket = Dummy_ticket {
        address = address
      ; payload = payload
      ; quantity = right_amount }
      in
      Some (left_ticket, right_ticket)
    else None