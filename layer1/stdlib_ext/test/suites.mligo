#import "test_listext.mligo" "Listext_test"
#import "test_stringext.mligo" "Stringext_test"
#import "test_optionext.mligo" "Optionext_test"
#import "test_atomic_test.mligo" "Atom_test"

let suites = 
    [ Listext_test.suite
    ; Stringext_test.suite
    ; Optionext_test.suite
    ; Atom_test.suite
    ]
