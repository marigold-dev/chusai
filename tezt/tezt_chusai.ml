open Tezt_chusai_lib

let protocols = [ Protocol.Jakarta ]

let () =
  Lwt_main.run @@ Default_parameters.gen ();
  Dummy_test.register ~protocols;
  Tezt.Test.run ()
;;
