open Chusai_lib
open Chusai_lib.Serializable

module MichelineTezos : MichelineT = struct

  open Tezos_micheline

  include Node

  type prim_node = Michelson_v1_primitives.prim node

  type t = Michelson_v1_primitives.prim Micheline.canonical

  let make_node n =
    let rec make_micheline_node = function
        Int i -> Micheline.Int (-1, i)
      | String s -> Micheline.String (-1, s)
      | Bytes b -> Micheline.Bytes (-1, b)
      | Prim (p, ns) -> Micheline.Prim (-1, p, List.map make_micheline_node ns, [])
      | Seq ns -> Micheline.Seq (-1, List.map make_micheline_node ns)
    in
    make_micheline_node n |> Micheline.strip_locations

  let from_node x = make_node x

  let encoding =
    Micheline_encoding.canonical_encoding
      ~variant:"michelson_v1"
      Michelson_v1_primitives.prim_encoding
end

module Pack = Make_Pack (MichelineTezos)
