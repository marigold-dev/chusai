type t =
  | Ithaca
  | Jakarta
  | Alpha

type constants =
  | Constants_sandbox
  | Constants_mainnet
  | Constants_test

type supported_protocols =
  | Any_protocol
  | From_protocol of int
  | Until_protocol of int
  | Between_protocols of int * int

type parameter_overrides = (string list * string option) list

let default_constants = Constants_sandbox

let name = function
  | Alpha -> "Alpha"
  | Ithaca -> "Ithaca"
  | Jakarta -> "Jakarta"
;;

let number = function
  | Ithaca -> 012
  | Jakarta -> 013
  | Alpha -> 014
;;

let directory = function
  | Alpha -> "proto_alpha"
  | Ithaca -> "proto_012_Psithaca"
  | Jakarta -> "proto_013_PtJakart"
;;

let tag protocol = String.lowercase_ascii (name protocol)

let hash = function
  | Alpha -> "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
  | Ithaca -> "Psithaca2MLRFYargivpo7YvUr7wUDqyxrdhC5CQq78mRvimz6A"
  | Jakarta -> "PtJakart2xVj7pYXJBXrqHgd82rdkLey5ZeeGwDgPp9rhQUbSqY"
;;

let genesis_hash = "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im"
let demo_counter_hash = "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT"

let parameter_file ?(constants = default_constants) protocol =
  let name =
    match constants with
    | Constants_sandbox -> "sandbox"
    | Constants_mainnet -> "mainnet"
    | Constants_test -> "test"
  in
  let directory = directory protocol in
  Format.asprintf "_build/%s/parameters/%s-parameters.json" directory name
;;

let node_name = function
  | Alpha -> "alpha"
  | Ithaca -> "012-Psithaca"
  | Jakarta -> "013-PtJakart"
;;

let next_protocol = function
  | Ithaca -> Some Jakarta
  | Jakarta -> None
  | Alpha -> None
;;

let previous_protocol = function
  | Alpha -> Some Jakarta
  | Jakarta -> Some Ithaca
  | Ithaca -> None
;;

let all = [ Alpha; Ithaca; Jakarta ]

let is_supported supported_protocols protocol =
  match supported_protocols with
  | Any_protocol -> true
  | From_protocol n -> number protocol >= n
  | Until_protocol n -> number protocol <= n
  | Between_protocols (a, b) ->
    let n = number protocol in
    a <= n && n <= b
;;

let show_supported_protocols = function
  | Any_protocol -> "Any_protocol"
  | From_protocol n -> Format.asprintf "From_protocol %d" n
  | Until_protocol n -> Format.asprintf "Until_protocol %d" n
  | Between_protocols (a, b) -> Format.asprintf "Between_protocol (%d, %d)" a b
;;

let iter_on_supported_protocols ~title ~protocols ?(supports = Any_protocol) f =
  match List.filter (is_supported supports) protocols with
  | [] ->
    failwith
      (Format.asprintf
         "test %s was registered with ~protocols:[%s] %s, which results in an \
          empty list of protocols"
         title
         (String.concat ", " (List.map name protocols))
         (show_supported_protocols supports))
  | supported_protocols -> List.iter f supported_protocols
;;

let add_to_test_parameters protocol title tags =
  name protocol ^ ": " ^ title, tag protocol :: tags
;;

let register_test ~__FILE__ ~title ~tags ?supports body protocols =
  let wrapped_test protocol =
    let title, tags = add_to_test_parameters protocol title tags in
    Tezt.Test.register ~__FILE__ ~title ~tags (fun () -> body protocol)
  in
  iter_on_supported_protocols ~title ~protocols ?supports wrapped_test
;;

let register_regression_test
    ~__FILE__
    ~title
    ~tags
    ?supports
    ~output_file
    body
    protocols
  =
  let wrapped_test protocol =
    let title, tags = add_to_test_parameters protocol title tags in
    Tezt.Regression.register
      ~__FILE__
      ~title
      ~tags
      ~output_file:(output_file protocol)
      (fun () -> body protocol)
  in
  iter_on_supported_protocols ~title ~protocols ?supports wrapped_test
;;

let write_parameter_file
    ?(additional_bootstrap_accounts = [])
    ~base
    parameter_overrides
  =
  let overriden_params = Tezt.Temp.file "parameters.json" in
  let original_params =
    let file =
      Either.fold
        ~left:Fun.id
        ~right:(fun (x, constants) -> parameter_file ?constants x)
        base
    in
    Tezt.JSON.(parse_file file |> unannotate)
  in
  let parameters =
    List.fold_left
      (fun acc (path, value) ->
        let parsed_value = Option.map Ezjsonm.value_from_string value in
        Ezjsonm.update acc path parsed_value)
      original_params
      parameter_overrides
  in
  let parameters =
    let bootstrap_accounts = [ "bootstrap_accounts" ] in
    let existing_accounts =
      Ezjsonm.get_list Fun.id (Ezjsonm.find parameters bootstrap_accounts)
    in
    let additional_bootstrap_accounts =
      List.map
        (fun (account, default_balance) ->
          `A
            [ `String account.Key.public_key_hash
            ; `String
                (string_of_int
                   (Option.value ~default:4000000000000 default_balance))
            ])
        additional_bootstrap_accounts
    in
    Ezjsonm.update
      parameters
      bootstrap_accounts
      (Some (`A (existing_accounts @ additional_bootstrap_accounts)))
  in
  Tezt.JSON.encode_to_file_u overriden_params parameters;
  Lwt.return overriden_params
;;
