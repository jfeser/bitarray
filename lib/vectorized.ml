open Base
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

let empty = { len = 0; buf = "" }
let fold = C.fold
let iter = C.iter
let length = C.length
let to_list = C.to_list
let to_array = C.to_array

let of_string len buf =
  assert (String.length buf * 8 >= len);
  { buf; len }

let to_string x = x.buf

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
  let lnot a =
    let buf = create_buf a.len in
    bitarray_not a.buf buf a.len;
    {
      a with
      buf = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf;
    }

  let ( lor ) = binary bitarray_or
  let ( land ) = binary bitarray_and
  let ( lxor ) = binary bitarray_xor
end

let any a = bitarray_any a.buf
let all a = bitarray_all a.buf a.len
let is_subset a ~of_:b = O.(all (lnot a lor b))

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
let init ~f len = init_fold ~f:(fun () i -> ((), f i)) ~init:() len
let of_array x = init (Array.length x) ~f:(Array.get x)
let of_list x = of_array @@ List.to_array x
let iter x f = iter x ~f

let iteri x ~f =
  let i = ref 0 in
  iter x (fun x ->
      f !i x;
      Int.incr i)

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

let init_bitmap ~w ~h ~f =
  assert (w >= 0 && h >= 0);
  let len = w * h in
  init_fold
    ~f:(fun (x, y) i ->
      let y' = if x = w - 1 then y - 1 else y in
      let x' = if x = w - 1 then 0 else x + 1 in
      ((x', y'), f ~i ~x ~y))
    ~init:(0, h - 1)
    len

let pp_bitmap ~w fmt x =
  let open Caml.Format in
  iteri x ~f:(fun i b ->
      if b then fprintf fmt "█" else fprintf fmt ".";
      if i % w = w - 1 then fprintf fmt "\n")

let hash_vec a = bitarray_hash a.buf
