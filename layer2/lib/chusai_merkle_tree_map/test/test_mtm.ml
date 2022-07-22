open Quickcheck_tests
open Example_tests

let () =
  let open Alcotest in
  run
    "MerkleTreeMap"
    [ "to_list", [ test_case "to_list empty equals empty list" `Quick test_empty ]
    ; ( "from_list"
      , [ test_case "to_list from_list []" `Quick test_from_empty_list
        ; test_case "to_list from_list one-elem-list" `Quick test_from_one_elem_list
        ; test_case "to_list from_list two-elem-list" `Quick test_from_two_elem_list
        ; test_case "to_list from_list three-elem-list" `Quick test_from_three_elem_list
        ] )
    ; ( "quickcheck - map operations"
      , [ QCheck_alcotest.to_alcotest quickcheck_test_to_list_from_list
        ; QCheck_alcotest.to_alcotest quickcheck_test_upsert_lookup
        ] )
    ; ( "proof - examples"
      , [ test_case "lookup found" `Quick test_verify_proof_lookup_found
        ; test_case "lookup not found" `Quick test_verify_proof_lookup_not_found
        ] )
    ; ( "proof - quickcheck"
      , [ QCheck_alcotest.to_alcotest quickcheck_test_verify_proof_lookup_found
        ; QCheck_alcotest.to_alcotest quickcheck_test_verify_proof_lookup_not_found
        ] )
    ]
;;
