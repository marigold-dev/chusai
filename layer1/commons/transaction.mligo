
(** Chusai contract *)
type chusai_contract = bytes

(** Storege of Chusai contract *)
type chusai_storage = bytes option

(** Number of operations emission from Chusai contract *)
type chusai_op_emission = nat

(** Return of Chusai contract *)
type chusai_contract_return = chusai_storage * chusai_op_emission

(** Balance of Chusai *)
type chusai_balance = nat


(** Chusai states represent ledgers, which consist of
    balance, contract, return from contract.

    TODO: replace with merkletree instead *)
type chusai_states = (address, chusai_balance * (chusai_contract_return * chusai_contract) option) map

(** [transaction] is an operation which means
    [source] transfers [quantity] amount to [destination],
    and applys a contract of [destination] with [arg]
    if the contract is existed.

    FIXME: needs signature
*)
type transaction =
  { source      : address
  ; destination : address
  ; quantity    : nat
  ; arg         : bytes option
  ;
  }

(** create a [transaction] operation *)
let make (source : address) (destination : address)
      (quantity : chusai_balance) (arg : bytes option) : transfer =
  { source = source; destination = destination; quantity = quantity; arg = arg }

(** create a [transaction] operation without [arg] for
    transfer amount only *)
let make_only_balance (source : address) (destination : address)
      (quantity : chusai_balance) : transfer =
  { source = source; destination = destination; quantity = quantity; arg = None }
