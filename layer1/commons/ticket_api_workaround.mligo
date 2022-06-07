(*

NOTE: This is a workaround for a LIGO compiler bug that doesn't allow use to run tests with tickets 
By comment/uncomment you can switch between real and dummy implementations

Steps:
1. switch to real implementation
2. write your code and make sure it compiles without problems
3. switch to dummy implementation
4. run the tests and make sure they are running without errors
5. switch back to real implementation
6. build your code again to make sure you didn't break anything during step 4
7. commit
*)

type chusai_payload = bytes

// Real impleentation that we use for testing the correctness in terms of linearity
////////////
// type chusai_ticket = chusai_payload ticket
// let create_ticket (b : bytes) (v : nat) : chusai_ticket = Tezos.create_ticket b v
// let join_tickets (l : chusai_ticket) (r : chusai_ticket) : chusai_ticket option= 
//  Tezos.join_tickets (l, r)
// let read_ticket (t : chusai_ticket) : (address * (bytes * nat)) * chusai_ticket =
//  Tezos.read_ticket t 
// let split_ticket (ticket : chusai_ticket) (amounts : nat * nat) : (chusai_ticket * chusai_ticket) option =
//  Tezos.split_ticket ticket amounts


// Dummy implementation that doesn't use tickets so we can actually run the tests even 
// with the current bug in the LIGO compiler
////////////////
type chusai_ticket = DummyTicket of (chusai_payload * nat)
let create_ticket (b : bytes) (v : nat) : chusai_ticket = DummyTicket (b, v)

let join_tickets (left : chusai_ticket) (right : chusai_ticket) : chusai_ticket option= 
 match (left, right) with
   (DummyTicket (bl,vl)), (DummyTicket (br, vr)) ->
    if bl = br then
      Some ((DummyTicket (bl, (vl + vr))) : chusai_ticket)
    else
      None

let read_ticket (t : chusai_ticket) : (address * (chusai_payload * nat)) * chusai_ticket =
 match t with
   DummyTicket (b, v) ->
     let some_address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) in 
       ((some_address, (b, v)), t)

let split_ticket (ticket : chusai_ticket) (amount_left, amount_right : nat * nat) : (chusai_ticket * chusai_ticket) option =
 match ticket with 
   DummyTicket (payload, ticket_value) ->
     if ticket_value = amount_left + amount_right
     then Some ((DummyTicket (payload, amount_left), DummyTicket (payload, amount_right)) : chusai_ticket * chusai_ticket)
     else None