(** Payload of a Ticket. *)
type payload = bytes

(** Monomorphic version a Tezos ticket parametrized with the payload. *)
type t = payload ticket

(** When a ticket has been read. *)
type been_read = (address * (payload * nat)) * t

(** When a ticket has beed splitted. *)
type splitted = (t * t) option

(** [Tezos_ticket.create_ticket payload qty]
    Create a [Ticket].
    FIXME: the parameter [address] is just mandatory for fixing the address
           of Chusai_ticket but is not used here. *)
let create_ticket (address: address) (payload: bytes) (quantity: nat) =
  Tezos.create_ticket payload quantity

(** [Tezos_ticket.read_ticket a_dummy_ticket]
    Read a [Tezos_ticket]. *)
let read (ticket: t) : been_read =
  Tezos.read_ticket ticket

(** [Tezos_ticket.join_tickets left_ticket right_ticket]
    Join two [Tezos_ticket] of the same kind. *)
let join (left: t) (right: t) : t option =
  Tezos.join_tickets (left, right)

(** [Tezos_ticket.split ticket left_part right_part]
    If [left_part + right_part] = ticket.quantity, it produces two
    new tickets. *)
let split_ticket (ticket: t) (left_amount: nat) (right_amount: nat) : splitted =
  Tezos.split_ticket ticket (left_amount, right_amount)