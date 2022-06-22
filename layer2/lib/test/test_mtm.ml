open Quickcheck_tests
open Example_tests

let () =
  let open Alcotest in
  run "MerkleTreeMap" [
    "to_list", [ 
      test_case "to_list empty equals empty list" `Quick test_empty 
    ];
    "from_list", [ 
      test_case "to_list from_list []" `Quick test_from_empty_list;
      test_case "to_list from_list one-elem-list" `Quick test_from_one_elem_list;
      test_case "to_list from_list two-elem-list" `Quick test_from_two_elem_list;
      test_case "to_list from_list three-elem-list" `Quick test_from_three_elem_list;
    ];
    "noop", [ 
      test_case "remove_from empty" `Quick test_remove_from_empty;
      test_case "remove missing key from non empty" `Quick test_remove_missing_key;
    ];
    "remove", [
      test_case "remove leaf" `Quick test_remove_leaf;
      test_case "remove node/right" `Quick test_remove_node_right;
      test_case "remove node/right/right" `Quick test_remove_node_right_right;
      test_case "remove node/right/left" `Quick test_remove_node_right_left;
      test_case "remove node/right/left/left_left" `Quick test_remove_node_right_left_left_left;
      test_case "remove node/right/left/y" `Quick test_remove_node_right_left_y;
      test_case "remove node/left" `Quick test_remove_node_left;
      test_case "remove node/left/left" `Quick test_remove_node_left_left;
      test_case "remove y node" `Quick test_remove_y_node;
      ];

      "proof examples", [
        test_case "insert in empty list" `Quick test_verify_proof_insert_in_empty_list;
        test_case "insert left in one_element list" `Quick test_verify_proof_insert_left_in_one_elem_list;
        test_case "insert right in one_element list" `Quick test_verify_proof_insert_right_in_one_elem_list;
        test_case "update one element list" `Quick test_verify_proof_update_one_elem_list;
        test_case "remove from empty map" `Quick test_verify_remove_from_empty_map;
        test_case "remove from nonempty map - interchanging proofs" `Quick test_verify_interchange_remove_proofs;
        test_case "remove existing key left" `Quick test_verify_remove_existing_key_left;
        test_case "remove existing key right" `Quick test_verify_remove_existing_key_right;
        test_case "remove non_leaf 2" `Quick test_verify_proof_remove_non_leaf_2;
        test_case "quickcheck failure 1" `Quick test_quickcheck_failure_1;
        test_case "quickcheck failure 2" `Quick test_quickcheck_failure_2;
      ];
      
      "quickcheck - verify proof", [
        QCheck_alcotest.to_alcotest quickcheck_test_verify_proof_insert;
        QCheck_alcotest.to_alcotest quickcheck_test_verify_proof_update;
        QCheck_alcotest.to_alcotest quickcheck_test_verify_proof_remove_failure;
        QCheck_alcotest.to_alcotest quickcheck_test_verify_proof_remove_leaf;
        QCheck_alcotest.to_alcotest quickcheck_test_verify_proof_remove_non_leaf;
        QCheck_alcotest.to_alcotest quickcheck_test_verify_proof_remove_non_leaf_2;
      ];

      "quickcheck - map operations", [
        QCheck_alcotest.to_alcotest quickcheck_test_to_list_from_list;
        QCheck_alcotest.to_alcotest quickcheck_test_remove;
        QCheck_alcotest.to_alcotest quickcheck_test_remove_upsert_remove;
      ]
  ]