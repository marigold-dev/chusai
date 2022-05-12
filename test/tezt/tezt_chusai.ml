open Tezt_chusai_lib

let protocols = [ Protocol.Alpha; Protocol.Jakarta; Protocol.Ithaca ]

let () =
  Lwt_main.run @@ Default_parameters.gen ();
  Dummy_test.register ~protocols;
  Tezt.Test.run ()
;;
