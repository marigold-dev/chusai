type hash_t = bytes

type ('k, 'v) map_verifier = {
  compare_key : 'k -> 'k -> int;
  hash_key : 'k -> hash_t;
  hash_value : 'v -> hash_t;
}