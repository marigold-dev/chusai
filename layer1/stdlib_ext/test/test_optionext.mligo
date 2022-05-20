#include "../src/stdlibext.mligo"

let test_OptionExt_bind =
  let _ = 
    let actual = 
      OptionExt.bind 
        (None : int option) 
        (fun (x : int) -> Some x) in

    assert (actual = (None : int option)) in

let _ = 
    let actual = 
      OptionExt.bind 
        (Some 10 : int option) 
        (fun (x : int) -> Some x) in

    assert (actual = Some 10) in

let _ = 
    let actual = 
      OptionExt.bind 
        (Some 10 : int option) 
        (fun (_ : int) -> (None : int option)) in

    assert (actual = (None : int option)) in

  unit