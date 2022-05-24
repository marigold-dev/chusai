type chusai_payload = bytes
type chusai_ticket = chusai_payload ticket

let create_ticket (b : bytes) (v : nat) : chusai_ticket = Tezos.create_ticket b v
let join_tickets (l : chusai_ticket) (r : chusai_ticket) : chusai_ticket option= 
 Tezos.join_tickets (l, r)
let read_ticket (t : chusai_ticket) : (address * (bytes * nat)) * chusai_ticket =
 Tezos.read_ticket t 
let split_ticket (ticket : chusai_ticket) (amounts : nat * nat) : (chusai_ticket * chusai_ticket) option =
 Tezos.split_ticket ticket amounts

