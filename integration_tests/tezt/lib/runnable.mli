type ('a, 'b) t =
  { value : 'a
  ; run : 'a -> 'b Lwt.t
  }

type 'a process = (Tezt.Process.t, 'a) t

val run : ('a, 'b) t -> 'b Lwt.t
val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

module Syntax : sig
  type nonrec ('a, 'b) t = ('a, 'b) t =
    { value : 'a
    ; run : 'a -> 'b Lwt.t
    }

  val ( let*! ) : ('a, 'b) t -> ('b -> 'c Lwt.t) -> 'c Lwt.t
  val ( let*? ) : ('a, 'b) t -> ('a -> 'c) -> 'c
end
