type ('a, 'b) t =
  { value : 'a
  ; run : 'a -> 'b Lwt.t
  }

type 'a process = (Tezt.Process.t, 'a) t

let run { value; run } = run value

let map f { value; run } =
  let run x =
    let open Lwt.Syntax in
    let* output = run x in
    Lwt.return (f output)
  in
  { value; run }
;;

module Syntax = struct
  type nonrec ('a, 'b) t = ('a, 'b) t =
    { value : 'a
    ; run : 'a -> 'b Lwt.t
    }

  let ( let*! ) x f =
    let open Lwt.Syntax in
    let* res = run x in
    f res
  ;;

  let ( let*? ) x f = f x.value
end
