open! Base
open Base_quickcheck
open Stubs

let[@inline] ceil_div x y = (x + y - 1) / y
let[@inline] read_bit w b = (Char.to_int w lsr b) land 1 > 0

let[@inline] write_bit w b v =
  Char.of_int_exn (Char.to_int w land lnot (1 lsl b) lor (Bool.to_int v lsl b))

let bits_per_word = 8
let bits_per_vword = 32

module T = struct
  module Elt = struct
    type t = bool [@@deriving equal]
  end

  type t = { buf : string; len : int } [@@deriving compare, equal, hash, sexp]

  let fold { len; buf } ~init ~f =
    if len = 0 then init
    else
      let i = ref 0
      and n_words = ceil_div len bits_per_word
      and state = ref init in
      for w = 0 to n_words - 2 do
        let word = buf.[w] in
        for b = 0 to bits_per_word - 1 do
          state := f !state (read_bit word b);
          Int.incr i
        done
      done;
      let last_word = buf.[n_words - 1] in
      for b = 0 to len - !i - 1 do
        state := f !state (read_bit last_word b);
        Int.incr i
      done;
      !state

  let iter = `Define_using_fold
  let length = `Custom (fun x -> x.len)
end

module C = Container.Make0 (T)
include T

let fold = C.fold
let iter = C.iter
let length = C.length
let to_list = C.to_list
let to_array = C.to_array

let[@inline] nwords len =
  ceil_div len bits_per_vword * (bits_per_vword / bits_per_word)

let[@inline] create_buf len = Bytes.make (nwords len) '\x00'

let create len init =
  let nwords = nwords len in
  let init_word = if init then '\xFF' else '\x00' in
  let buf = String.make nwords init_word in
  { buf; len }

let random len =
  let nwords = nwords len in
  let buf = String.init nwords ~f:(fun _ -> Random.char ()) in
  { buf; len }

let[@inline] binary op a b =
  assert (a.len = b.len);
  let buf = create_buf a.len in
  op a.buf b.buf buf;
  {
    a with
    buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf;
  }

let[@inline] unary op a =
  let buf = create_buf a.len in
  op a.buf buf;
  {
    a with
    buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf;
  }

module O = struct
  let lnot = unary bitarray_not
  let ( lor ) = binary bitarray_or
  let ( land ) = binary bitarray_and
  let ( lxor ) = binary bitarray_xor
end

let any a = bitarray_any a.buf
let all a = bitarray_all a.buf

let init_fold ~f ~init len =
  let buf = create_buf len in
  let return_buf buf =
    {
      buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf;
      len;
    }
  in
  if len = 0 then return_buf buf
  else
    let nwords = ceil_div len bits_per_word in
    let i = ref 0 and state = ref init in
    for w = 0 to nwords - 2 do
      let x = ref 0 in
      for b = 0 to bits_per_word - 1 do
        let state', value = f !state !i in
        x := !x + (Bool.to_int value lsl b);
        Int.incr i;
        state := state'
      done;
      Bytes.set buf w @@ Char.of_int_exn !x
    done;
    let x = ref 0 in
    for b = 0 to len - !i - 1 do
      let state', value = f !state !i in
      x := !x + (Bool.to_int value lsl b);
      Int.incr i;
      state := state'
    done;
    Bytes.set buf (nwords - 1) @@ Char.of_int_exn !x;
    return_buf buf

let get t i =
  if i < 0 || i >= t.len then
    raise_s [%message "index out of bounds" (i : int) (t.len : int)];
  let w = i / bits_per_word and b = i % bits_per_word in
  read_bit t.buf.[w] b

let set t i v =
  if i < 0 || i >= t.len then
    raise_s [%message "index out of bounds" (i : int) (t.len : int)];
  let w = i / bits_per_word and b = i % bits_per_word in
  let buf' = Bytes.of_string t.buf in
  Bytes.set buf' w (write_bit t.buf.[w] b v);
  {
    t with
    buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf';
  }

let one_hot ~len x = set (create len false) x true
let init ~f x = Shared.init ~init_fold ~f x

let iteri x ~f =
  let i = ref 0 in
  iter x ~f:(fun x ->
      f !i x;
      Int.incr i)

let of_list x = Shared.of_list ~init_fold x
let hamming_weight a = bitarray_hamming_weight a.buf

let[@inline] hamming_distance a b =
  assert (a.len = b.len);
  bitarray_hamming_distance a.buf b.buf

let jaccard_distance a b = bitarray_jaccard a.buf b.buf

let replicate ~w ~h t ~dx ~dy ~ct =
  assert (w >= 0 && h >= 0 && t.len = w * h && ct >= 0);
  let buf = create_buf t.len in
  bitarray_replicate t.buf dx dy ct w h buf;
  {
    buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf;
    len = t.len;
  }

let corners ~w ~h t =
  assert (w >= 0 && h >= 0 && t.len = w * h);
  let buf = create_buf t.len in
  bitarray_corners t.buf w h buf;
  {
    buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf;
    len = t.len;
  }

let init_bitmap = Shared.init_bitmap init_fold
let pp_bitmap = Shared.pp_bitmap iteri

let%test_unit "hamming-weight" =
  for _ = 0 to 100 do
    let len = Random.int_incl 8 256 in
    let bits = List.init len ~f:(fun _ -> Random.bool ()) in
    let expect = List.sum (module Int) bits ~f:(fun b -> if b then 1 else 0) in
    let v = of_list bits in
    let actual = hamming_weight v in
    [%test_result: int] ~expect actual
  done

module Blocked_matrix = struct
  type nonrec t = { m_buf : string; m_dim : int; m_bit_dim : int }
  [@@deriving compare, equal, hash, sexp, quickcheck]

  let quickcheck_shrinker = Shrinker.atomic

  let quickcheck_generator =
    let open Generator.Let_syntax in
    let%bind m_bit_dim = Generator.int_inclusive 1 8 in
    let round_bit_dim = Int.round_up ~to_multiple_of:8 m_bit_dim in
    let m_dim = round_bit_dim / 8 in
    let%bind m_buf = Generator.string_with_length ~length:(m_dim * m_dim * 8) in
    return { m_dim; m_buf; m_bit_dim }

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

  let get m i j =
    let n = m.m_dim in
    let bit_dim = bit_dim m in
    assert (0 <= i && i < bit_dim && 0 <= j && j < bit_dim);
    (* coordinates of the block and inside the block *)
    let block_i = i / 8 and block_j = j / 8 in
    let inner_i = i % 8 and inner_j = j % 8 in
    (* index of the block (in words) *)
    let block_idx = (block_i * n) + block_j in
    (* index into the block (in bits) *)
    let inner_idx = (inner_i * 8) + inner_j in
    (* index into the buffer *)
    let byte_idx = (block_idx * 8) + (inner_idx / 8) in
    [%test_pred: int * int]
      (fun (idx, len) -> 0 <= idx && idx < len)
      (byte_idx, String.length m.m_buf);
    read_bit m.m_buf.[byte_idx] (inner_idx % 8)

  let set m i j v =
    let n = m.m_dim in
    let bit_dim = bit_dim m in
    if i < 0 || i >= bit_dim || j < 0 || j >= bit_dim then
      raise_s
        [%message
          "matrix index out of bounds" (i : int) (j : int) (bit_dim : int)];
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
      bitarray_not a.m_buf buf;
      {
        a with
        m_buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf;
      }
  end

  let rec pow a n =
    assert (n > 0);
    if n = 1 then a
    else if n % 2 = 1 then O.(a * pow a (n - 1))
    else
      let p = pow a (n / 2) in
      O.(p * p)
  (* let pow a n = *)
  (*   assert (n > 0); *)
  (*   if n = 1 then a *)
  (*   else if n = 2 then O.(a * a) *)
  (*   else if n = 3 then O.(a * a * a) *)
  (*   else if n = 4 then *)
  (*     O.( *)
  (*       let aa = a * a in *)
  (*       aa * aa) *)
  (*   else *)
  (*     let buf = Bytes.make (String.length a.m_buf) '\x00' in *)
  (*     bitarray_pow a.m_buf buf a.m_dim n; *)
  (*     { *)
  (*       a with *)
  (*       m_buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf; *)
  (*     } *)

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
  (* let result = Bytes.of_string (pow a l).m_buf in *)
  (* let scratch = Bytes.make (String.length a.m_buf) '\x00' in *)
  (* for _ = l + 1 to h do *)
  (*   let result_s = Bytes.to_string result in *)
  (*   bitarray_mul a.m_buf result_s scratch a.m_dim; *)
  (*   let scratch_s = *)
  (*     Bytes.unsafe_to_string ~no_mutation_while_string_reachable:scratch *)
  (*   in *)
  (*   bitarray_or result_s scratch_s result *)
  (* done; *)
  (* { *)
  (*   a with *)
  (*   m_buf = *)
  (*     Bytes.unsafe_to_string ~no_mutation_while_string_reachable:result; *)
  (* } *)

  let jaccard_distance a b =
    assert (a.m_dim = b.m_dim);
    bitarray_jaccard a.m_buf b.m_buf

  (* let to_matrix a = *)
  (*   let n = a.m_dim in *)
  (*   let b = Array.make_matrix ~dimx:n ~dimy:n false in *)
  (*   let rec block_loop block_start block_row block_col = *)
  (*     for i = 0 to 7 do *)
  (*       let c = Char.to_int a.m_buf.[block_start + i] in *)
  (*       for j = 0 to 7 do *)
  (*         b.(block_row + i).(block_col + j) <- (c lsr (7 - j)) land 0x1 > 0 *)
  (*       done *)
  (*     done; *)
  (*     if block_start + 8 < String.length a.m_buf then *)
  (*       let block_row, block_col = *)
  (*         if block_col + 8 >= n then (block_row + 8, 0) *)
  (*         else (block_row, block_col + 8) *)
  (*       in *)
  (*       block_loop (block_start + 8) block_row block_col *)
  (*   in *)
  (*   block_loop 0 0 0; *)
  (*   b *)
end
