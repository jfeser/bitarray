open Base

type t = int [@@deriving compare, equal, hash, sexp]

let len_bits = 6
let len_mask = 0b111111
let data_bits = Int.num_bits - 6

let create n v =
  assert (0 <= n && n <= data_bits);
  ((if v then -1 else 0) lsl len_bits) lor (n land len_mask)

let[@inline] length x = x land len_mask

let get x i =
  assert (0 <= i && i < length x);
  (x lsr (i + len_bits)) land 1 <> 0

let set x i v =
  assert (0 <= i && i < length x);
  let mask = lnot (1 lsl (i + len_bits)) in
  x land mask lor (Bool.to_int v lsl (i + len_bits))

let any x = x lsr len_bits <> 0
let to_list x = List.init (length x) ~f:(get x)
let one_hot ~len i = set (create len false) i true

module O = struct
  let ( land ) a b =
    assert (length a = length b);
    a land b

  let ( lor ) a b =
    assert (length a = length b);
    a lor b
end
