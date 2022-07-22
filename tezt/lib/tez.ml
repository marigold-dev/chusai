type t = int64

let of_int amount = Int64.(mul 1_000_000L (of_int amount))
let of_mutez_int = Int64.of_int
let zero = 0L
let one = of_int 1
let mutez_int64 t = t
let of_z amount = amount |> Z.to_int |> of_int

let to_string amount =
  let mult_int = 1_000_000L in
  let rec left amount =
    let d, r = Int64.(div amount 1000L), Int64.(rem amount 1000L) in
    if d > 0L then Format.asprintf "%s%03Ld" (left d) r else Format.asprintf "%Ld" r
  in
  let right amount =
    let triplet v =
      if v mod 10 > 0
      then Format.asprintf "%03d" v
      else if v mod 100 > 0
      then Format.asprintf "%02d" (v / 10)
      else Format.asprintf "%d" (v / 100)
    in
    let hi, lo = amount / 1000, amount mod 1000 in
    if lo = 0
    then Format.asprintf "%s" (triplet hi)
    else Format.asprintf "%03d%s" hi (triplet lo)
  in
  let ints, decs = Int64.(div amount mult_int), Int64.(to_int (rem amount mult_int)) in
  if decs > 0 then Format.asprintf "%s.%s" (left ints) (right decs) else left ints
;;

let to_float amount = Float.mul (Int64.to_float amount) 0.000_001
let to_mutez amount = Int64.to_int amount
let to_z tez = tez |> to_mutez |> Z.of_int
let ( + ) = Int64.add
let ( - ) = Int64.sub

let parse_floating tez_string =
  let re = Tezt.Base.rex "(\\d+)\\.?(\\d*)" in
  let fail () = Tezt.Test.fail "Invalid tez value: '%s'." tez_string in
  let parse_int s =
    match int_of_string_opt s with
    | None -> fail ()
    | Some i -> i
  in
  let integral, decimal =
    match Tezt.Base.(tez_string =~** re) with
    | None -> fail ()
    | Some (i, d) -> i, d
  in
  let integral = parse_int integral in
  let decimal =
    match String.length decimal with
    | 0 -> 0
    | 1 -> 100_000 * parse_int decimal
    | 2 -> 10_000 * parse_int decimal
    | 3 -> 1_000 * parse_int decimal
    | 4 -> 100 * parse_int decimal
    | 5 -> 10 * parse_int decimal
    | 6 -> parse_int decimal
    | _ -> fail ()
  in
  of_int integral + of_mutez_int decimal
;;
