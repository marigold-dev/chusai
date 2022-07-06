let print (msg : string) (a : 'a) : unit =
  let _ = Format.printf "%s: <<%s>>\n" msg (Batteries.dump a) in
  ()
;;
