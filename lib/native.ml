open! Base

type buf = int array [@@deriving compare, sexp]

let hash_fold_buf = Hash.Builtin.(hash_fold_array_frozen hash_fold_int)

type t = { len : int; buf : buf } [@@deriving compare, hash, sexp]

let length t = t.len
let bits_per_word = 63
let nwords len = (len / bits_per_word) + if len % bits_per_word > 0 then 1 else 0

let create len v =
  let fill = if v then 0x7FFFFFFFFFFFFFFF else 0 in
  { len; buf = Array.create ~len:(nwords len) fill }

let init_fold ~f ~init len =
  let nwords = nwords len in
  let buf = Array.create ~len:nwords 0 in
  let idx = ref 0 in
  let state = ref init in
  for w = 0 to nwords - 1 do
    for b = 0 to bits_per_word - 1 do
      if !idx < len then (
        let state', value = f !state !idx in
        let v = if value then 1 else 0 in
        buf.(w) <- buf.(w) + (v lsl b);
        Int.incr idx;
        state := state')
    done
  done;
  { len; buf }

let fold { len; buf } ~f ~init =
  let i = ref 0 and n_words = Array.length buf and state = ref init in
  for w = 0 to n_words - 2 do
    let word = buf.(w) in
    for b = 0 to bits_per_word - 1 do
      let bit = (word lsr b) land 1 > 0 in
      state := f !state bit;
      Int.incr i
    done
  done;
  let last_word = buf.(n_words - 1) in
  for b = 0 to len - !i - 1 do
    let bit = (last_word lsr b) land 1 > 0 in
    assert (!i < len);
    state := f !state bit;
    Int.incr i
  done;
  !state

let init = Shared.init ~init_fold
let of_list = Shared.of_list ~init_fold
let iteri = Shared.iteri ~fold
let get t i = (t.buf.(i / bits_per_word) lsr (i % bits_per_word)) land 1 > 0
let to_list x = List.init (length x) ~f:(get x)
let is_empty a = Array.exists a.buf ~f:(fun x -> x <> 0)
let not a = { a with buf = Array.map a.buf ~f:lnot }
let and_ a b = { len = a.len; buf = Array.map2_exn a.buf b.buf ~f:( land ) }
let or_ a b = { len = a.len; buf = Array.map2_exn a.buf b.buf ~f:( lor ) }
let xor a b = { len = a.len; buf = Array.map2_exn a.buf b.buf ~f:( lxor ) }
let hamming_weight x = Array.sum (module Int) x.buf ~f:Int.popcount

let hamming_distance a b =
  Array.fold2_exn a.buf b.buf ~f:(fun ct w w' -> ct + Int.popcount (w lxor w')) ~init:0

let weighted_jaccard ?(pos_weight = 0.5) a b =
  ((Float.of_int (hamming_weight (not @@ and_ a b)) *. pos_weight)
  +. (Float.of_int (hamming_weight (not @@ and_ (not a) (not b))) *. (1.0 -. pos_weight))
  )
  /. (Float.of_int @@ length a)

let%expect_test "init-in-bounds" =
  let len = 99 in
  (init len ~f:(fun i ->
       [%test_pred: int] (fun i -> 0 <= i && i < len) i;
       false)
    : t)
  |> ignore

let%expect_test "iteri-in-bounds" =
  let len = 99 in
  create len false
  |> iteri ~f:(fun i _ -> [%test_pred: int] (fun i -> 0 <= i && i < len) i)

let%expect_test "" =
  for _ = 0 to 100 do
    let l = List.init ~f:(fun _ -> Random.bool ()) 256 in
    [%test_result: bool list] ~expect:l (of_list l |> to_list)
  done

let%expect_test "" =
  for i = 0 to 10 do
    let b = init 10 ~f:(fun j -> j < i) in
    [%test_result: int] ~expect:i (hamming_weight b)
  done

let init_bitmap = Shared.init_bitmap init_fold
let pp_bitmap = Shared.pp_bitmap iteri
let offset ~w ~h ~x ~y = ((h - 1 - y) * w) + x

let replicate_is_set ~w ~h scene dx dy count x y =
  let rec loop count x y =
    if count <= 0 then false
    else
      ((x >= 0 && x < w && y >= 0 && y < h) && get scene (offset ~w ~h ~x ~y))
      || loop (count - 1) (x - dx) (y - dy)
  in
  loop count x y

let replicate ~w ~h s ~dx ~dy ~ct =
  init_bitmap ~w ~h ~f:(fun ~i:_ ~x ~y -> replicate_is_set ~w ~h s dx dy ct x y)

let corners ~w:_ ~h:_ _ = failwith "unimplemented"

let[@inline] jaccard c c' =
  let union = hamming_weight (or_ c c') in
  (* the empty scene is equal to itself *)
  if union = 0 then 0.0
  else 1.0 -. Float.(of_int (hamming_weight (and_ c c')) / of_int union)
