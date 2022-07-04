open Hash_algo
open Format

module Sha256 : Hash_algo.Hash = 
  struct
    type t = Sha256 of string

    let pp (formatter : formatter) ((Sha256 s) : t) : unit =
      Format.fprintf formatter "%s" s

    let equal (Sha256 left) (Sha256 right) = left = right
    
    let serialize ( value : 'a) : string = Batteries.dump value
    let hash  (value : 'v) : t = 
      let s = serialize value in
      let hasher = Cryptokit.Hash.sha256() in
      let _ = String.iter (fun b -> hasher#add_char b) s in
      let (Tezos_base58.Base58 enc) = 
        hasher#result 
        |> Tezos_base58.encode ~prefix:"" in
      Sha256 enc
    
    let (++) (Sha256 left) (Sha256 right) = hash @@ String.cat left right
    let empty : t = hash "sha256_empty"
  end