#import "stdlibext.mligo" "Stdlib"
#include "../test-framework/context.mligo"
#import "../test-framework/engine.mligo" "Engine"
#include "../test-framework/status.mligo"
#include "../test-framework/assertions.mligo"
#include "../test-framework/wrap_test.mligo"
#include "../test-framework/contract.mligo"
    

let make_test = Engine.make_test
let make_suite = Engine.make_suite
let run_suites = Engine.run_suites