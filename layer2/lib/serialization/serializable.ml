
module Node = struct
  type 'a node = 
    | Int of Z.t
    | String of string
    | Bytes of Bytes.t
    | Prim of 'a * 'a node list
    | Seq of 'a node list
end

module type NODE = sig
  include (module type of Node)
end

module type MICHELINE = sig

  include NODE

  type prim_node = Michelson_v1_primitives.prim Node.node

  type t

  val from_node : prim_node -> t

  val encoding : t Data_encoding.encoding
end

module MichelineLite : MICHELINE = struct
  include Node

  type prim_node = Michelson_v1_primitives.prim node

  type t = prim_node

  let from_node x = x

  (* It is reimplemented
     {{: https://ocaml.org/p/tezos-micheline/12.3/doc/Tezos_micheline/Micheline/index.html#type-node} canonical_encoding from tezos_micheline} lib but without locations
     and annotations, which aren't required in chusai. *)
  let encoding : t Data_encoding.encoding =
    let open Data_encoding in
    let int_encoding tag =
      case
        tag
        (obj1 (req "int" z))
        ~title:"Int"
        (function Int v -> Some v | _ -> None)
        (fun v -> Int v)
    in
    let bytes_encoding tag =
      case
        tag
        ~title:"Bytes"
        (obj1 (req "bytes" bytes))
        (function Bytes v -> Some v | _ -> None)
        (fun v -> Bytes v)
    in
    let string_encoding tag =
      case
        tag
        ~title:"String"
        (obj1 (req "string" string))
        (function String v -> Some v | _ -> None)
        (fun v -> String v)
    in
    let seq_encoding tag expr_encoding =
      case
        tag
        (list expr_encoding)
        ~title:"Sequence"
        (function Seq v -> Some v | _ -> None)
        (fun args -> Seq args)
    in
    let prim_no_arg_encoding tag =
      case
        tag
        ~title:"Prim__no_args__no_annots"
        ~description:"Primitive with no arguments and no annotations"
        (obj1 (req "prim" Michelson_v1_primitives.prim_encoding))
        (function Prim (v, []) -> Some v | _ -> None)
        (fun v -> Prim (v, []))
    in
    let prim_1_arg_encoding tag expr_encoding =
      case
        tag
        ~title:"Prim__1_arg__no_annots"
        ~description:"Primitive with one argument and no annotations"
        (obj2 (req "prim" Michelson_v1_primitives.prim_encoding) (req "arg" expr_encoding))
        (function
          | Prim (v, [arg]) -> Some (v, arg) | _ -> None)
        (function prim, arg -> Prim (prim, [arg]));
    in
    let prim_2_arg_encoding tag expr_encoding =
      case
        tag
        ~title:"Prim__2_args__no_annots"
        ~description:
          "Primitive with two arguments and no annotations"
        (obj3
           (req "prim" Michelson_v1_primitives.prim_encoding)
           (req "arg1" expr_encoding)
           (req "arg2" expr_encoding))
        (function
          | Prim (prim, [arg1; arg2]) ->
            Some (prim, arg1, arg2)
          | _ -> None)
        (fun (prim, arg1, arg2) -> Prim (prim, [arg1; arg2]))
    in
    let prim_encoding tag expr_encoding =
      case
        tag
        ~title:"Prim__generic"
        ~description:
          "Generic primitive (any number of args with or without annotations)"
        (obj3
           (req "prim" Michelson_v1_primitives.prim_encoding)
           (dft "args" (list expr_encoding) [])
           (dft "annots" string ""))
        (function
          | Prim (prim, args) -> Some (prim, args, "") | _ -> None)
        (fun (prim, args, _) -> Prim (prim, args))
    in
    mu
      "micheline.michelson_v1.expression"
      (fun expr_encoding ->
         union
           ~tag_size:`Uint8
           [ int_encoding (Tag 0);
             string_encoding (Tag 1);
             seq_encoding (Tag 2) expr_encoding;
             prim_no_arg_encoding (Tag 3);
             prim_1_arg_encoding (Tag 5) expr_encoding;
             prim_2_arg_encoding (Tag 7) expr_encoding;
             prim_encoding (Tag 9) expr_encoding;
             bytes_encoding (Tag 10);
           ])
end

module Ty = struct
  type 'a ty =
    | Int_t : Z.t ty
    | Bytes_t : Bytes.t ty
    | String_t : string ty
    | Pair_t : 'a ty * 'b ty -> ('a * 'b) ty
    | Tuple_t : 'a ty * 'b ty * 'c ty -> ('a * 'b* 'c) ty
    | List_t : 'a ty -> 'a list ty
end

module type TY = sig
  include (module type of Ty)
end

module type PACK = sig
  include TY

  val pack : 'a ty * 'a -> bytes
end

module Make_pack (M : MICHELINE) : PACK
= struct
  include Ty

  type micheline_liked = M.prim_node

  (** check {{https://tezos.gitlab.io/shell/micheline.html} here} for
      the spec of micheline. *)
  let rec to_micheline_liked : 
    type a. a ty -> a -> micheline_liked =
    fun ty x ->
    match ty with
    | Int_t -> Int x
    | Bytes_t -> Bytes x
    | String_t -> String x
    | Pair_t (tl, tr) ->
      let (l, r) = x in
      let xl = to_micheline_liked tl l in
      let xr = to_micheline_liked tr r in
      Prim (D_Pair, [xl; xr])
    | Tuple_t (t1, t2, t3) ->
      let (x1, x2, x3) = x in
      let x1 = to_micheline_liked t1 x1 in
      let x2 = to_micheline_liked t2 x2 in
      let x3 = to_micheline_liked t3 x3 in
      Prim (D_Pair, [x1; x2; x3])
    | List_t t ->
      Seq (List.map (to_micheline_liked t) x)

  let pack_micheline (node : M.t) : bytes =
    let b = Data_encoding.Binary.to_bytes_exn M.encoding node in
    Bytes.cat (Bytes.of_string "\005") b

  let pack : type a. a ty * a -> bytes = 
    fun (ty, data) ->
    to_micheline_liked ty data 
    |> M.from_node 
    |> pack_micheline
end

module Pack = Make_pack (MichelineLite)
