open Intf
open Format

module Make (Serializer : SERIALIZER) : HASH = struct
  type t = string

  let pp (formatter : formatter) (sha : t) : unit = Format.fprintf formatter "%s" sha
  let equal left right = left = right

  let hash (value : 'v) : t =
    let open Tezos_base58 in
    let h =
      Serializer.serialize value |> Hacl_star.Hacl.SHA3_256.hash |> String.of_bytes
    in
    let (Base58 enc) = encode ~prefix:"" h in
    enc
  ;;

  let ( ++ ) left right = hash @@ left ^ right
  let empty : t = hash "sha256_empty"
end
