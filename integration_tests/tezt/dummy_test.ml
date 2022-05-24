open Tezt_chusai_lib

let test_dummy =
  Protocol.register_test
    ~__FILE__
    ~title:"A dummy test"
    ~tags:[]
    (fun protocol ->
      let open Lwt.Syntax in
      let () = Tezt.Log.info "Initialize Node and Client" in
      let* _node, _client =
        Client.init_with_protocol ~base_dir:"_build" ~protocol ()
      in
      Lwt.return_unit)
;;

let register ~protocols = test_dummy protocols
