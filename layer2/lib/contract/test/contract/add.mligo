(** to generate michelson code
    >  ligo compile contract --protocol ithaca add.mligo > add.tz
*)

(** bytes should be able to unpack as nat *)
type storage = bytes option

(** bytes should be able to unpack as nat *)
type parameter = bytes

type return = operation list * storage

(** this contract deserizlize bytes of parameter and storage
    , add deserizlized results, pack the result, and store to
    storage. If deserizlization fail, [None] will store to
    storage *)
let main((p, o_s) : parameter * storage) : return =
  let o_p = (Bytes.unpack p : nat option) in
  let new_s = (match o_p, o_s with
  | None, _ -> (None : bytes option)
  | _, None -> (None : bytes option)
  | Some p, Some b_s -> (
     match (Bytes.unpack b_s : nat option) with
     | None -> (None : bytes option)
     | Some s -> Some (Bytes.pack (p + s))))
  in
  ([], new_s)
