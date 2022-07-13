open Tezos_protocol_013_PtJakart.Protocol
open Tezos_error_monad.Error_monad

type error += Cannot_serialize_storage

val wrap_tztrace_lwt : ('a, Environment.Error_monad.error Environment.Error_monad.trace) result Lwt.t -> ('a, error trace) result Lwt.t

val interp : Script_typed_ir.logger option -> Alpha_context.t * Script_typed_ir.step_constants -> ('a, 'b) Script_typed_ir.lambda -> 'a -> ('b * Alpha_context.t) tzresult Lwt.t
