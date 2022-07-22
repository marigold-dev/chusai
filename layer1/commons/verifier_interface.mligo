type hash_t = bytes


type 'k proof_step =
  | GoLeft of
      { key : 'k
      ; vhash : hash_t
      ; rhash : hash_t
      }
  | GoRight of
      { key : 'k
      ; vhash : hash_t
      ; lhash : hash_t
      }
  | NotFound 
  | Found of
      { key : 'k
      ; vhash : hash_t
      ; lhash : hash_t
      ; rhash : hash_t
      }

type 'k proof = 'k proof_step list