let base_path = "_build/layer1"
let locate = Filename.concat base_path
let root = "./"

let resolve_path contract =
  let real_path = locate contract in
  Filename.concat root real_path
;;

let originate
  ~name
  ?(amount = Tez.zero)
  ~originator
  ?(burn_cap = Tez.of_int 9999)
  ~file
  ~init
  client
  =
  Client.originate_contract
    ~alias:name
    ~amount
    ~src:originator
    ~burn_cap
    ~prg:file
    ~init
    client
;;

let originate_mint
  ?(name = "mint_sc")
  ~originator
  ?amount
  ?burn_cap
  ?(fixed_payload = "0x00")
  ?(minimal_accepted_quantity = Tez.of_int 1)
  client
  =
  let init =
    Format.asprintf "(Pair %s %d)" fixed_payload @@ Tez.to_mutez minimal_accepted_quantity
  in
  let file = resolve_path "mint_sc.tez" in
  originate ~name ?amount ~originator:originator.Key.alias ?burn_cap ~file ~init client
;;

let originate_inbox
  ?(name = "inbox_sc")
  ~originator
  ?amount
  ?burn_cap
  ?(fixed_payload = "0x00")
  mint_address
  client
  =
  let init =
    Format.asprintf {|(Pair 0 None (Pair "%s" %s) {})|} mint_address fixed_payload
  in
  let file = resolve_path "inbox_sc.tez" in
  originate ~name ?amount ~originator:originator.Key.alias ?burn_cap ~file ~init client
;;

type rollup_scs =
  { mint_sc : string
  ; inbox_sc : string
  }

let originate_rollup_scs
  ~originator
  ?(fixed_payload = "0x00")
  ?(minimal_accepted_quantity = Tez.of_int 1)
  client
  =
  let open Lwt.Syntax in
  let* mint_sc =
    originate_mint ~originator ~fixed_payload ~minimal_accepted_quantity client
  in
  (* Only one manager operation per block is allowed. *)
  let* () = Client.bake_for_and_wait client in
  let* inbox_sc = originate_inbox ~originator ~fixed_payload mint_sc client in
  Lwt.return { mint_sc; inbox_sc }
;;

let originate_wallet
  ?(name = "wallet_sc")
  ?amount
  ?burn_cap
  ~originator
  mint_address
  inbox_address
  client
  =
  let init =
    Format.asprintf
      {|(Pair "%s" "%s" "%s" None)|}
      originator.Key.public_key_hash
      mint_address
      inbox_address
  in
  let file = resolve_path "wallet_sc.tez" in
  originate ~name ?amount ~originator:originator.Key.alias ?burn_cap ~file ~init client
;;

let transfert_to_contract
  ?(burn_cap = Tez.of_int 999)
  ~originator
  ~contract_address
  ~amount
  arg
  client
  =
  let giver = originator.Key.alias
  and receiver = contract_address in
  Client.transfer ~burn_cap ~arg ~amount ~giver ~receiver client
;;

let wallet_request_mint ?burn_cap ~originator ~wallet_address amount client =
  transfert_to_contract
    ~originator
    ?burn_cap
    ~contract_address:wallet_address
    ~amount
    "(Left (Left (Left Unit)))"
    client
;;

let wallet_deposit ?burn_cap ~originator ~wallet_address client =
  transfert_to_contract
    ~originator
    ?burn_cap
    ~contract_address:wallet_address
    ~amount:Tez.zero
    "(Right Unit)"
    client
;;
