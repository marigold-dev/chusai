open Tezos_protocol_013_PtJakart.Protocol.Alpha_context
open Tezos_error_monad.Error_monad

(** Parse a Michelson expression from a file
    of chusai contract, raising an exception on error. *)
val from_file : string -> Script.expr

(** Serialize chusai contract as bytes, which
    can be deserizlized as lambda function on layer 1 *)
val pack_chusai_contract : Script.expr -> bytes tzresult Lwt.t
