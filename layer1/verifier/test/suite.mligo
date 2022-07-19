#import "../../stdlib_ext/src/unit_test.mligo" "Unit"
#import "test_insert.mligo" "Test_insert"

let suite = Unit.make_suite
  "verifier"
  "Test suite for Verifier"
  [ 
    Unit.make_test "Insert" "insert in empty map" Test_insert._test_insert_in_empty_map 
  ]