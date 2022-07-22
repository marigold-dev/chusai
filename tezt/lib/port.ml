let next = ref 0

let fresh () =
  let slot = !next mod 1000 in
  incr next;
  Tezt.Cli.options.starting_port
  + (1000 * (Tezt.Test.current_worker_id () |> Option.value ~default:0))
  + slot
;;

let () = Tezt.Test.declare_reset_function @@ fun () -> next := 0
