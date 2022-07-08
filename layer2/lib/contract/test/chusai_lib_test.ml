open Chusai_contract.Contract
open Tezos_error_monad.Error_monad.Legacy_monad_globals

let of_bytes s = (Bytes.of_string (Hex.to_string (`Hex s)))


let test_pack_chusai_contract () =
  let contract_expr = from_file "./contract/add.tz" in
  pack_chusai_contract contract_expr
  >>=? fun result ->
  (* see pack_add.mligo to generate expected result *)
  let expected = of_bytes "05020000004e037a050d0362072f02000000060320053e03690200000030034c072f02000000060320053e0369020000001c050d0362072f02000000060320053e036902000000060312030c0346053d036d0342"
  in
  Alcotest.(check bytes) "didn't match" result expected |> return

let () =
  let open Tezos_base_test_helpers.Tztest in
  Alcotest_lwt.run "Utils" [
    "Chusai Contract", [tztest "test pack chusai contract" `Quick test_pack_chusai_contract]
  ] |> Lwt_main.run
