open Base
open Vectorized
open Stubs

type nonrec t = { m_buf : string; m_dim : int; m_bit_dim : int }
[@@deriving compare, equal, hash, sexp]

module Private = struct
  let unsafe_create m_buf m_dim m_bit_dim = { m_buf; m_dim; m_bit_dim }
end

let create m_bit_dim v =
  let round_bit_dim = Int.round_up ~to_multiple_of:8 m_bit_dim in
  let block_width = round_bit_dim / 8 in
  let n_blocks = block_width * block_width in
  let n_bytes = n_blocks * 8 in
  let init = if v then '\xFF' else '\x00' in
  { m_dim = block_width; m_buf = String.make n_bytes init; m_bit_dim }

let dim m = m.m_bit_dim
let[@inline] n_bits m = m.m_dim * 64
let[@inline] bit_dim m = m.m_bit_dim

let check_bounds m i j =
  let dim = bit_dim m in
  if i < 0 || i >= dim || j < 0 || j >= dim then
    raise_s
      [%message "matrix index out of bounds" (i : int) (j : int) (dim : int)]

let get m i j =
  check_bounds m i j;
  let n = m.m_dim in
  (* coordinates of the block and inside the block *)
  let block_i = i / 8 and block_j = j / 8 in
  let inner_i = i % 8 and inner_j = j % 8 in
  (* index of the block (in words) *)
  let block_idx = (block_i * n) + block_j in
  (* index into the block (in bits) *)
  let inner_idx = (inner_i * 8) + inner_j in
  (* index into the buffer *)
  let byte_idx = (block_idx * 8) + (inner_idx / 8) in
  read_bit m.m_buf.[byte_idx] (inner_idx % 8)

let set m i j v =
  check_bounds m i j;
  let n = m.m_dim in
  (* coordinates of the block and inside the block *)
  let block_i = i / 8 and block_j = j / 8 in
  let inner_i = i % 8 and inner_j = j % 8 in
  (* index of the block (in words) *)
  let block_idx = (block_i * n) + block_j in
  (* index into the block (in bits) *)
  let inner_idx = (inner_i * 8) + inner_j in
  (* index into the buffer *)
  let byte_idx = (block_idx * 8) + (inner_idx / 8) in
  let bit_idx = inner_idx % 8 in
  let byte' = write_bit m.m_buf.[byte_idx] bit_idx v in
  let buf' = Bytes.of_string m.m_buf in
  Bytes.set buf' byte_idx byte';
  {
    m with
    m_buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf';
  }

let iter x f =
  for i = 0 to dim x - 1 do
    for j = 0 to dim x - 1 do
      f (i, j, get x i j)
    done
  done

let identity n =
  let rec set_diag m i =
    if i < n then set_diag (set m i i true) (i + 1) else m
  in
  set_diag (create n false) 0

let upper_triangle n =
  let a = ref (create n false) in
  for i = 0 to n - 1 do
    for j = i to n - 1 do
      a := set !a i j true
    done
  done;
  !a

let to_matrix a =
  let bit_dim = bit_dim a in
  let b = Array.make_matrix ~dimx:bit_dim ~dimy:bit_dim false in
  for i = 0 to bit_dim - 1 do
    for j = 0 to bit_dim - 1 do
      b.(i).(j) <- get a i j
    done
  done;
  b

let of_matrix a =
  let bit_dim = Array.length a in
  let b = ref (create bit_dim false) in
  for i = 0 to bit_dim - 1 do
    for j = 0 to bit_dim - 1 do
      if a.(i).(j) then b := set !b i j true
    done
  done;
  !b

let to_bitarray a = { buf = a.m_buf; len = a.m_bit_dim * a.m_bit_dim }

module O = struct
  let ( * ) a b =
    assert (a.m_dim = b.m_dim);
    let buf = Bytes.make (String.length a.m_buf) '\x00' in
    bitarray_mul a.m_buf b.m_buf buf a.m_dim;
    {
      a with
      m_buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf;
    }

  let ( land ) a b =
    assert (a.m_dim = b.m_dim);
    let buf = Bytes.make (String.length a.m_buf) '\x00' in
    bitarray_and a.m_buf b.m_buf buf;
    {
      a with
      m_buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf;
    }

  let ( lor ) a b =
    assert (a.m_dim = b.m_dim);
    let buf = Bytes.make (String.length a.m_buf) '\x00' in
    bitarray_or a.m_buf b.m_buf buf;
    {
      a with
      m_buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf;
    }

  let ( lxor ) a b =
    assert (a.m_dim = b.m_dim);
    let buf = Bytes.make (String.length a.m_buf) '\x00' in
    bitarray_xor a.m_buf b.m_buf buf;
    {
      a with
      m_buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf;
    }

  let lnot a =
    let buf = Bytes.make (String.length a.m_buf) '\x00' in
    bitarray_not a.m_buf buf Int.(a.m_bit_dim * a.m_bit_dim);
    {
      a with
      m_buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf;
    }
end

let rec pow a n =
  if n <= 0 then raise_s [%message "expected positive" (n : int)];
  if n = 1 then a
  else if n % 2 = 1 then O.(a * pow a (n - 1))
  else
    let p = pow a (n / 2) in
    O.(p * p)

let transitive_range a l h =
  assert (l <= h);
  if l = h then pow a l
  else
    let rec loop or_acc mul_acc n =
      if n = h then O.(or_acc lor mul_acc)
      else loop O.(or_acc lor mul_acc) O.(mul_acc * a) (n + 1)
    in
    let acc = pow a l in
    loop acc acc l

let jaccard_distance a b =
  assert (a.m_dim = b.m_dim);
  bitarray_jaccard a.m_buf b.m_buf
