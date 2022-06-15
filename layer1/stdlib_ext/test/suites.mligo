#import "test_listext.mligo" "Listext_test"
#import "test_stringext.mligo" "Stringext_test"
#import "test_optionext.mligo" "Optionext_test"
#import "test_unit_test.mligo" "Test_framework"

let suites = 
    [ Listext_test.suite
    ; Stringext_test.suite
    ; Optionext_test.suite
    ; Test_framework.suite
    ]
