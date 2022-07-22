open Tezos_base.TzPervasives

let get_balance_for address node =
  let open Lwt_syntax in
  let service = Format.asprintf "balances/%s" address in
  let+ json = Chusai_node.rpc ~service node in
  match json with
  | None -> Z.zero
  | Some x ->
    (match Tezt.JSON.as_opt x with
    | None -> Z.zero
    | Some x -> Tezt.JSON.as_int x |> Z.of_int)
;;
