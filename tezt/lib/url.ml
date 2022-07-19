type method_ =
  | GET
  | PUT
  | POST
  | PATCH
  | DELETE

let encode str =
  let buffer = Buffer.create (String.length str * 3) in
  let () =
    String.iter
      (function
        | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-' | '/') as c ->
          Buffer.add_char buffer c
        | c ->
          Buffer.add_char buffer '%';
          let c1, c2 = Hex.of_char c in
          Buffer.add_char buffer c1;
          Buffer.add_char buffer c2)
      str
  in
  let result = Buffer.contents buffer in
  let () = Buffer.reset buffer in
  result
;;

let method_to_string = function
  | GET -> "get"
  | PUT -> "put"
  | POST -> "post"
  | PATCH -> "patch"
  | DELETE -> "delete"
;;

let path_to_string path = "/" ^ String.concat "/" (List.map encode path)

let query_to_string = function
  | [] -> ""
  | qs ->
    let qs' = List.map (fun (k, v) -> encode k, encode v) qs in
    "?" ^ String.concat "&" @@ List.map (fun (k, v) -> k ^ "=" ^ v) qs'
;;

let rpc_path ?(query_string = []) path =
  let path = path_to_string path in
  let query = query_to_string query_string in
  path ^ query
;;
