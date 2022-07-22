let () =
  let open Alcotest in
  run "Utils" [
    Test_pack.Ligo_bytes_repr.test ();
    Test_tezos_pack.Tezos_micheline_repr.test ()
  ]
