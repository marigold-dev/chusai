
let list_with_key_gen (karb : 'k QCheck.arbitrary) (varb : 'v QCheck.arbitrary): (('k * 'v) * ('k * 'v) list) QCheck.arbitrary = 
  let open QCheck in
  triple (pair karb varb) (list_of_size (int_range 0 0).gen (pair karb varb)) (list_of_size (int_range 1 1).gen (pair karb varb))
  |> map (fun ((k,v), lst1, lst2) -> 
    let lst = Tools.unique @@ List.concat [lst1; [(k,v)]; lst2] in
    ((k, v), lst))

let nonempty_small_list (arb : 'v QCheck.arbitrary): 'v list QCheck.arbitrary = 
  let open QCheck in
  pair arb (small_list arb)
  |> map (fun (h, t) -> h :: t)