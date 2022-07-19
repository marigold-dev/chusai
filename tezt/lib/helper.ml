(* MIT License

   Copyright (c) 2022 Marigold <contact@marigold.dev>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
   the Software, and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

type participant =
  { name : string
  ; key : Key.t
  ; wallet_address : string
  }

let init_wallets rollup_sc tezos_client =
  let open Lwt.Syntax in
  let mint_address = rollup_sc.Chusai_contract.mint_sc
  and inbox_address = rollup_sc.Chusai_contract.inbox_sc in
  let* alice =
    let name = "alice" in
    let key = Key.sample.(1) in
    let+ wallet_address =
      Chusai_contract.originate_wallet
        ~name:"wallet_sc_alice"
        ~originator:key
        mint_address
        inbox_address
        tezos_client
    in
    name, { name; key; wallet_address }
  in
  let* () = Client.bake_for_and_wait tezos_client in
  let* bob =
    let name = "bob" in
    let key = Key.sample.(2) in
    let+ wallet_address =
      Chusai_contract.originate_wallet
        ~name:"wallet_sc_bob"
        ~originator:key
        mint_address
        inbox_address
        tezos_client
    in
    name, { name; key; wallet_address }
  in
  let* () = Client.bake_for_and_wait tezos_client in
  let* carol =
    let name = "carol" in
    let key = Key.sample.(2) in
    let+ wallet_address =
      Chusai_contract.originate_wallet
        ~name:"wallet_sc_carol"
        ~originator:key
        mint_address
        inbox_address
        tezos_client
    in
    name, { name; key; wallet_address }
  in
  let* () = Client.bake_for_and_wait tezos_client in
  Lwt.return (Chusai_common.Map.String.of_seq (List.to_seq [ alice; bob; carol ]))
;;

let register_test ?before_chusai_node ~__FILE__ ~title ~tags test =
  Protocol.register_test ~__FILE__ ~title ~tags (fun protocol ->
    let open Lwt.Syntax in
    let () = Tezt.Log.info "Initialize Node and Client" in
    let operator = Key.sample.(0) in
    let* tezos_node, tezos_client = Client.init_with_protocol ~protocol () in
    let* () = Node.Tezos.wait_for_ready tezos_node in
    let* rollup_sc =
      Chusai_contract.originate_rollup_scs ~originator:operator tezos_client
    in
    let* () = Client.bake_for_and_wait tezos_client in
    let* participants = init_wallets rollup_sc tezos_client in
    let* () =
      match before_chusai_node with
      | None -> Lwt.return_unit
      | Some f -> f protocol tezos_node tezos_client rollup_sc participants
    in
    let chusai_node =
      Chusai_node.create
        ~operator
        ~inbox_address:rollup_sc.Chusai_contract.inbox_sc
        tezos_client
        tezos_node
    in
    let* () = Chusai_node.run chusai_node in
    let* () = Chusai_node.wait_for_ready chusai_node in
    let* () = Client.bake_for_and_wait tezos_client in
    test protocol tezos_node tezos_client rollup_sc participants chusai_node)
;;

let request_mint participants name amount client =
  let x = Chusai_common.Map.String.(participants.%[name]) in
  let originator = x.key in
  let wallet_address = x.wallet_address in
  Chusai_contract.wallet_request_mint
    ~originator
    ~wallet_address
    (Tez.of_int amount)
    client
;;

let wallet_deposit participants name client =
  let x = Chusai_common.Map.String.(participants.%[name]) in
  let originator = x.key in
  let wallet_address = x.wallet_address in
  Chusai_contract.wallet_deposit ~originator ~wallet_address client
;;
