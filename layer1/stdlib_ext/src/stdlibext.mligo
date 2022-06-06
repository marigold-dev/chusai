module Prelude =
  struct
    [@inline]
    let undefined (type a) () : a =
      failwith "UNDEFINED" 
  end

module OptionExt =
  struct
    [@inline]
    let bind (type a b) (ma : a option) (f : a -> b option) : b option = 
      match ma with
        None -> None 
        | Some v -> f v

    [@inline]
    let or_else (type a) (opt1 : a option) (opt2 : a option) : a option =
      match opt1 with
        None -> opt2
        | Some _ -> opt1

    [@inline]
    let default (type a) (opt : a option) (default : a) : a =
      match opt with
        None -> default
        | Some v -> v

    [@inline]
    let is_none (type a) (opt : a option) : bool =
      match opt with
        None -> true
        | Some _ -> false

    [@inline]
    let is_some (type a) (opt : a option) : bool =
      match opt with
        None -> false
        | Some _ -> true

    [@inline]
    let iterate (type a) (opt : a option) (f : a -> unit):  unit =
      match opt with 
        None -> unit
        | Some a -> f a
  end


module ListExt =
  struct
    let reverse (type a) (xs : a list) : a list =
      let acc (ys,x : (a list * a)) : a list = x :: ys in
      List.fold_left acc ([] : a list) xs

    let concat (type a) (left : a list) (right : a list) : a list =
      let acc (x,ys : (a * a list)) : a list = x :: ys in
      List.fold_right acc left right

    let concat_all (type a) (lists : a list list) = 
      let aux (i,acc : a list * a list ) = concat i acc in
      List.fold_right aux lists ([]: a list)

    let join (type a) (lists : a list list) : a list =
      let acc (xs, ys : a list * a list) : a list = concat xs ys in
      List.fold_right acc lists ([] : a list)

    let bind (type a b) (f : a -> b list) (xs : a list) : b list =
      join (List.map f xs)

    let rec find (type a) (predicate : a -> bool) (xs : a list) : a option = 
      match xs with
        [] -> None
        | (h :: t) -> 
          if predicate h 
          then Some h 
          else find predicate t

    let filter (type a) (predicate : a -> bool) (xs : a list) : a list =
      let acc (a, acc : a * a list) = 
        if predicate a 
        then a :: acc
        else acc in
      List.fold_right acc xs ([] : a list)

    [@inline]
    let exists (type a) (predicate : a -> bool) (xs : a list) : bool =
      OptionExt.is_some (find predicate xs)

    [@inline]
    let cat_options (type a) (opts : (a option) list) : a list =
      List.map 
        (fun (opt : a option) -> Option.unopt opt) // Workaround for compiler bug
        (filter 
          (fun (opt : a option) -> OptionExt.is_some opt) 
          opts)

    [@inline]
    let sum (ints : int list) = 
      List.fold (fun (a, i : int * int) -> a + i) ints

    let sequence_options (type a) (opts : a option list) : a list option =
      let rec go (opts : a option list) (acc : a list) : a list option =
        match opts with 
          [] -> Some acc
          | (None :: _) -> None
          | (Some h :: t) -> go t (h :: acc) in
      Option.map (reverse : a list -> a list) (go opts ([] : a list))

    let intercalate (type a) (sep : a) (xs : a list) : a list =
      match xs with
      [] -> []
      | (h :: t) -> h :: bind (fun (x : a) -> [sep; x]) t

  end


module StringExt =
  struct
    [@inline]
    let concat_all (separator : string) (strings : string list) = 
      List.fold 
        (fun (acc, s : string * string) -> String.concat acc s)
        (ListExt.intercalate separator strings)
        ""
  end


module Function =
  struct
    [@inline]
    let curry (type a b c) (f : a * b -> c) : a -> b -> c =
      fun (a : a) ->  fun (b : b) -> f (a,b)
      
    [@inline]
    let uncurry (type a b c) (f : a -> b -> c) : a * b -> c =
      fun (a, b : a * b) -> f a b
  end
