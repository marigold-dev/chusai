open Tezos_crypto

type t =
  { alias : string
  ; public_key_hash : string
  ; public_key : string
  ; secret_key : string
  }

let use ~watermark ~signer bytes =
  let secret_key = Signature.Secret_key.of_b58check_exn signer.secret_key in
  Signature.sign ~watermark secret_key bytes
;;

let public_key_hash_dir = "public_key_hashs"
let public_key_dir = "public_keys"
let secret_key_dir = "secret_keys"

let write_keys directory subdir view keys =
  let filename = Filename.concat directory subdir in
  let json =
    `A
      (List.map
         (fun key -> `O [ "name", `String key.alias; "value", `String (view key) ])
         keys)
  in
  Tezt.JSON.encode_to_file_u filename json
;;

let write ~directory keys =
  write_keys directory public_key_hash_dir (fun x -> x.public_key_hash) keys;
  write_keys directory public_key_dir (fun x -> "unencrypted:" ^ x.public_key) keys;
  write_keys directory secret_key_dir (fun x -> "unencrypted:" ^ x.secret_key) keys
;;

let activator =
  { alias = "activator"
  ; public_key_hash = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"
  ; public_key = "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2"
  ; secret_key = "edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6"
  }
;;

let sample =
  [| { alias = "bootstrap1"
     ; public_key_hash = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
     ; public_key = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"
     ; secret_key = "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh"
     }
   ; { alias = "bootsrap2"
     ; public_key_hash = "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN"
     ; public_key = "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9"
     ; secret_key = "edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo"
     }
   ; { alias = "boostrap3"
     ; public_key_hash = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
     ; public_key = "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV"
     ; secret_key = "edsk4ArLQgBTLWG5FJmnGnT689VKoqhXwmDPBuGx3z4cvwU9MmrPZZ"
     }
   ; { alias = "bootstrap4"
     ; public_key_hash = "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv"
     ; public_key = "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU"
     ; secret_key = "edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3"
     }
   ; { alias = "bootstrap5"
     ; public_key_hash = "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv"
     ; public_key = "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n"
     ; secret_key = "edsk4QLrcijEffxV31gGdN2HU7UpyJjA8drFoNcmnB28n89YjPNRFm"
     }
  |]
;;

let all = activator :: Array.to_list sample
