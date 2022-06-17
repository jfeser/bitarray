external bitarray_and : string -> string -> bytes -> unit = "bitarray_and_stub"
  [@@noalloc]

external bitarray_or : string -> string -> bytes -> unit = "bitarray_or_stub"
  [@@noalloc]

external bitarray_xor : string -> string -> bytes -> unit = "bitarray_xor_stub"
  [@@noalloc]

external bitarray_any : string -> bool = "bitarray_any_stub" [@@noalloc]
external bitarray_all : string -> bool = "bitarray_any_stub" [@@noalloc]

external bitarray_not : string -> bytes -> unit = "bitarray_not_stub"
  [@@noalloc]

external bitarray_hamming_weight : string -> (int[@untagged])
  = "bitarray_hamming_weight_stub_byte" "bitarray_hamming_weight_stub"
  [@@noalloc]

external bitarray_hamming_distance : string -> string -> (int[@untagged])
  = "bitarray_hamming_distance_stub_byte" "bitarray_hamming_distance_stub"
  [@@noalloc]

external bitarray_jaccard : string -> string -> (float[@unboxed])
  = "bitarray_jaccard_stub_byte" "bitarray_jaccard_stub"
  [@@noalloc]

external bitarray_replicate :
  string ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  bytes ->
  unit = "bitarray_replicate_stub_byte" "bitarray_replicate_stub"
  [@@noalloc]

external bitarray_corners :
  string -> (int[@untagged]) -> (int[@untagged]) -> bytes -> unit
  = "bitarray_corners_stub_byte" "bitarray_corners_stub"
  [@@noalloc]

external bitarray_hash : string -> string -> (int[@untagged])
  = "bitarray_hash_stub_byte" "bitarray_hash_stub"
  [@@noalloc]

external bitarray_mul : string -> string -> bytes -> int -> unit
  = "bitarray_mul_stub"
  [@@noalloc]
