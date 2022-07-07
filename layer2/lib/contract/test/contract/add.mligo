(* to generate michelson code
    >  ligo compile contract --protocol ithaca add.mligo > add.tz
*)

(* nat *)
type storage = bytes
(* nat *)
type parameter = bytes
type return = operation list * storage

let main((p, s) : parameter * storage) : return =
  let o_p = (Bytes.unpack p : nat option) in
  let o_s = (Bytes.unpack s : nat option) in
  let new_s = match o_p, n_s with
  | None, _ -> None
  | _, None -> None
  | Some p, Some s -> Some p + s
  ([], new_s)
