#import "../stdlib_ext/src/atomic_test.mligo" "Atom"
#import "../mint/test/test_mint_sc.mligo" "Mint"

let test = Atom.run_suites [
    Mint.suite
  // ; Mint.suite 
]