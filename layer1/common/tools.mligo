
module OptionExt =
  struct
    let bind (type a b) (ma : a option) (f : a -> b option) : b option = 
      match ma with
        None -> None 
        | Some v -> f v

    let or_else (type a) (opt1 : a option) (opt2 : a option) : a option =
      match opt1 with
        None -> opt2
        | Some _ -> opt1

    let default (type a) (opt : a option) (default : a) : a =
      match opt with
        None -> default
        | Some v -> v

    let is_none (type a) (opt : a option) : bool =
      match opt with
        None -> true
        | Some _ -> false

    let is_some (type a) (opt : a option) : bool =
      match opt with
        None -> false
        | Some _ -> true
  end
