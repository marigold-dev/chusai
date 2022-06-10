#include "engine.mligo"
(* ********************************* *)
(* METRICS *)
(** Tools to run metrics, which are described same as tests, in terms of actions (unit -> result). *)

(** Run a set of metrics and pretty print their results on stdout. if
    there is a failure, the function raise an error. *)
let run_test_suite_metric (suite : test_suite) = 
  run_test_suite_with_pp pp_metric suite

(** Run a set of metrics and pretty print their results on stdout. if
    there is a failure, the function raise an error. *)
let run_suites_metrics  (suites : test_suite list) = 
  run_suites_with_pp pp_metric suites