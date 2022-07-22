open Tezos_base.TzPervasives

module Curl = struct
  let path_cache = ref None

  let get () =
    let open Tezt.Process in
    let open Lwt_syntax in
    try
      let* path =
        match !path_cache with
        | Some path -> return path
        | None ->
          let* path = run_and_read_stdout "sh" [ "-c"; "command -v curl" ] in
          let path = String.trim path in
          let () = path_cache := Some path in
          return path
      in
      let f ~url = run_and_read_stdout path [ "-s"; url ] in
      return_some f
    with
    | _ -> return None
  ;;
end
