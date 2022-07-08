(* to generate michelson code
    >  ligo compile contract --protocol ithaca add.mligo > add.tz
*)

(* nat *)
type storage = bytes option
(* nat *)
type parameter = bytes
type return = operation list * storage

let _main((p, o_s) : parameter * storage) : return =
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

let test =
  let b = (Bytes.pack _main) in
  match (Bytes.unpack b : ((bytes * bytes option -> operation list * storage) option)) with
  | Some _ -> Test.log "some"
  | None -> Test.log "none"
