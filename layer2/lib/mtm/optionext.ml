module OptionExt : sig
  val fold_lazy : none:(unit -> 'b) -> some:('a -> 'b) -> 'a option -> 'b
end = struct
  let fold_lazy ~none ~some opt : 'b =
    match opt with
    | None -> none ()
    | Some x -> some x
  ;;
end
