(** The abstract syntax tree of Micheline expressions. It's similary
    to the one in {{: https://ocaml.org/p/tezos-micheline/12.3/doc/Tezos_micheline/Micheline/index.html#type-node} tezos_micheline} but without locations and annotations.
    The only parameter is the type of primitive names.
*)
module type NodeT = sig
  type 'a node =
    | Int of Z.t
    | String of string
    | Bytes of Bytes.t
    | Prim of 'a * 'a node list
    | Seq of 'a node list
end

module Node : sig
  type 'a node =
    | Int of Z.t
    | String of string
    | Bytes of bytes
    | Prim of 'a * 'a node list
    | Seq of 'a node list
end

(** MichelineT provide a way to perform serialization
    of Michelson_v1_primitives. [prime_node] is to use to
    construct a micheline AST, [from_node] tranforms to
    [t], which can be serialized by [encoding]. *)
module type MichelineT = sig

  include NodeT

  type prime_node = Michelson_v1_primitives.prim Node.node

  type t

  val from_node : prime_node -> t

  val encoding : t Data_encoding.encoding
end

(** The type, corresponding to ocaml, is used to transplie
    ocaml data to Micheline AST, {!module:NodeT}. *)
module type TyT = sig
  type 'a ty =
    | Int_t : Z.t ty
    | Bytes_t : Bytes.t ty
    | String_t : string ty
    | Pair_t : 'a ty * 'b ty -> ('a * 'b) ty
    | Tuple_t : 'a ty * 'b ty * 'c ty -> ('a * 'b* 'c) ty
    | List_t : 'a ty -> 'a list ty
end

module Ty : sig
  type 'a ty =
    | Int_t : Z.t ty
    | Bytes_t : Bytes.t ty
    | String_t : string ty
    | Pair_t : 'a ty * 'b ty -> ('a * 'b) ty
    | Tuple_t : 'a ty * 'b ty * 'c ty -> ('a * 'b* 'c) ty
    | List_t : 'a ty -> 'a list ty
end

(** Performing the same serialization as the
    {{: https://tezos.gitlab.io/michelson-reference/#instr-PACK } pack}
     of Michelson instruction. *)
module type PackT = sig
  include TyT

  val pack : 'a ty * 'a -> bytes
end

(** Functor building an implementation of the pack
    with {!module:MichelineT}. *)
module Make_Pack : functor (M : MichelineT) -> PackT

(** Pack serializes certain ocaml data as bytes as same as the
    {{: https://tezos.gitlab.io/michelson-reference/#instr-PACK } pack}
    of Michelson instruction. [Pack.pack] doesn't use
    {{: https://ocaml.org/p/tezos-micheline/12.3/doc/Tezos_micheline/Micheline/index.html} tezos_micheline}
     lib to perform serialize. Instead, it
    reimplementations of only needed part of tezos_micheline
    and avoids unnecessary conversion between data.


    Examples of usage of serialization:
    - pack Int_t, Z.zero
    - pack String_t, "string"
    - pack (Pair_t Int_t, String), (Z.one, "pack")
    - pack (List_t (Pair_t Int_t, String_t)), [(Z.one, "pack"), (Z.zero, "again")]
    - pack (List_t Int_t), [Z.one]
*)
module Pack : sig
  (** ['a ty] describes types corresponding to ocaml type for pack *)
  type 'a ty =
    | Int_t : Z.t ty
    | Bytes_t : bytes ty
    | String_t : string ty
    | Pair_t : 'a ty * 'b ty -> ('a * 'b) ty
    | Tuple_t : 'a ty * 'b ty * 'c ty -> ('a * 'b * 'c) ty
    | List_t : 'a ty -> 'a list ty

  (** [pack] performs serialization. the type of providing data ['a] should
      match the type ['a ty]. *)
  val pack : 'a ty * 'a -> bytes
end
