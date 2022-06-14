let () =
  let open Alcotest in
  run "Utils" [
    Test_pack.Pack.test ();
    Test_tezos_pack.Pack_Tezos.test ()
  ]
