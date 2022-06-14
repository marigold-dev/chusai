
(** The functionality is the same as Pack in
   {!module:Serializable} but this implementation
   relies on
   {{: https://ocaml.org/p/tezos-micheline/12.3/doc/Tezos_micheline/Micheline/index.html} tezos_micheline}
   . *)
module Pack: sig
  type 'a ty =
    | Int_t : Z.t ty
    | Bytes_t : Bytes.t ty
    | String_t : string ty
    | Pair_t : 'a ty * 'b ty -> ('a * 'b) ty
    | Tuple_t : 'a ty * 'b ty * 'c ty -> ('a * 'b* 'c) ty
    | List_t : 'a ty -> 'a list ty

  val pack : 'a ty * 'a -> bytes
end
