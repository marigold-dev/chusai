module Curl : sig
  val get : unit -> (url:string -> string Lwt.t) option Lwt.t
end
