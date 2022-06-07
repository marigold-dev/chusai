let fold_lazy (none_fn : unit -> 'b) (some_fn : 'a -> 'b) (opt : 'a option) : 'b =
  match opt with
  | None -> none_fn ()
  | Some x -> some_fn x