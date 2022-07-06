open Format

module Make (Serializer : Serializer_algo.SERIALIZER) : Hash_algo.HASH = struct
  type t = Sha256 of string

  let pp (formatter : formatter) (Sha256 s : t) : unit = Format.fprintf formatter "%s" s
  let equal (Sha256 left) (Sha256 right) = left = right

  let hash (value : 'v) : t =
    let open Tezos_base58 in
    let h =
      Serializer.serialize value |> Hacl_star.Hacl.SHA3_256.hash |> String.of_bytes
    in
    let (Base58 enc) = encode ~prefix:"" h in
    Sha256 enc
  ;;

  let ( ++ ) (Sha256 left) (Sha256 right) = hash @@ left ^ right
  let empty : t = hash "sha256_empty"
end
