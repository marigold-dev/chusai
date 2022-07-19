open Tezt_chusai_lib

let test_dummy =
  Helper.register_test
    ~__FILE__
    ~title:
      "A dummy test that expect every smart-contracts can be originate and that the \
       state is properly initiated"
    ~tags:[ "dummy"; "chusai" ]
    ~before_chusai_node:(fun _protocol _tezos_node tezos_client _ participants ->
      let open Lwt.Syntax in
      let* () = Helper.request_mint participants "alice" 20 tezos_client in
      let* () = Client.bake_for_and_wait tezos_client in
      let* () = Helper.wallet_deposit participants "alice" tezos_client in
      let* () = Client.bake_for_and_wait tezos_client in
      Lwt.return_unit)
    (fun _protocol _tezos_node _tezos_client _rollup_sc participants chusai_node ->
      let alice = Chusai_common.Map.String.(participants.%["alice"]).wallet_address in
      let bob = Chusai_common.Map.String.(participants.%["bob"]).wallet_address in
      let open Lwt.Syntax in
      let* alice_balance = Tezt_chusai_lib.Chusai_rpc.get_balance_for alice chusai_node in
      let* bob_balance = Tezt_chusai_lib.Chusai_rpc.get_balance_for bob chusai_node in
      let () =
        let open Tezt.Check in
        ( = )
          alice_balance
          (Tez.of_int 20 |> Tez.to_z)
          Util.z
          ~error_msg:"Alice balance should be equal to 20tez"
      in
      let () =
        let open Tezt.Check in
        ( = ) bob_balance Z.zero Util.z ~error_msg:"Bob balance should be equal to 0tez"
      in
      Lwt.return_unit)
;;

let register ~protocols = test_dummy protocols
