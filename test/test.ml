open Base
open Base_quickcheck
open Bitarray
module V = Vectorized
module N = Native

module Bit_list = struct
  type t = bool list [@@deriving compare, quickcheck, sexp]
end

module Bit_list_nonempty = struct
  type t = bool list [@@deriving compare, quickcheck, sexp]

  let quickcheck_generator = Generator.(list_non_empty bool)
end

module Bit_list_equal_len_pair = struct
  type t = bool list * bool list [@@deriving compare, quickcheck, sexp]

  let quickcheck_shrinker = Shrinker.atomic

  let quickcheck_generator =
    let open Generator.Let_syntax in
    let%bind l1 = Bit_list.quickcheck_generator in
    let l2 = List.map l1 ~f:(fun _ -> Random.bool ()) in
    return (l1, l2)
end

let make_unary_test fn fv =
  Test.run_exn
    ~f:(fun a ->
      let n = N.of_list a and v = V.of_list a in
      [%test_result: Bit_list.t] ~expect:(N.to_list @@ fn n) (V.to_list @@ fv v))
    (module Bit_list)

let make_binary_test fn fv =
  Test.run_exn
    ~f:(fun (a, a') ->
      let n = N.of_list a
      and n' = N.of_list a'
      and v = V.of_list a
      and v' = V.of_list a' in
      [%test_result: Bit_list.t] ~expect:(N.to_list @@ fn n n') (V.to_list @@ fv v v'))
    (module Bit_list_equal_len_pair)

let make_binary_float_test fn fv =
  Test.run_exn
    ~f:(fun (a, a') ->
      let n = N.of_list a
      and n' = N.of_list a'
      and v = V.of_list a
      and v' = V.of_list a' in
      [%test_result: float] ~expect:(fn n n') (fv v v'))
    (module Bit_list_equal_len_pair)

let%test_unit "and" = make_binary_test N.and_ V.and_
let%test_unit "or" = make_binary_test N.or_ V.or_
let%test_unit "xor" = make_binary_test N.xor V.xor
let%test_unit "not" = make_unary_test N.not V.not
let%test_unit "jaccard" = make_binary_float_test N.jaccard V.jaccard

let%test_unit "get" =
  Test.run_exn
    ~f:(fun a ->
      let n = N.of_list a and v = V.of_list a and idx = Random.int (List.length a) in
      [%test_result: bool] ~expect:(N.get n idx) (V.get v idx))
    (module Bit_list_nonempty)

let%test_unit "hamming weight" =
  Test.run_exn
    ~f:(fun a ->
      let n = N.of_list a and v = V.of_list a in
      [%test_result: int] ~expect:(N.hamming_weight n) (V.hamming_weight v))
    (module Bit_list)

let%test_unit "hamming distance" =
  Test.run_exn
    ~f:(fun (a, a') ->
      let n = N.of_list a
      and n' = N.of_list a'
      and v = V.of_list a
      and v' = V.of_list a' in
      [%test_result: int] ~expect:(N.hamming_distance n n') (V.hamming_distance v v'))
    (module Bit_list_equal_len_pair)

let w = 16
and h = 16

(* let%test_unit "replicate" = *)
(*   Test.run_exn *)
(*     ~f:(fun a -> *)
(*       let n = N.of_list a *)
(*       and v = V.of_list a *)
(*       and dx = Random.int_incl (-5) 5 *)
(*       and dy = Random.int_incl (-5) 5 *)
(*       and ct = Random.int_incl 1 5 in *)
(*       let n_ret = N.replicate ~w ~h ~dx ~dy ~ct n *)
(*       and v_ret = V.replicate ~w ~h ~dx ~dy ~ct v in *)
(*       if Core.not ([%compare.equal: Bit_list.t] (N.to_list n_ret) (V.to_list v_ret)) then ( *)
(*         Format.printf "Dx=%d Dy=%d Ct=%d\nInput:\n%a\nExpected:\n%a\nGot:\n%a\n@." dx dy *)
(*           ct (N.pp_bitmap ~w) n (N.pp_bitmap ~w) n_ret (V.pp_bitmap ~w) v_ret; *)
(*         assert false)) *)
(*     (module struct *)
(*       type t = bool list [@@deriving compare, quickcheck, sexp] *)

(*       let quickcheck_generator = Generator.(list_with_length ~length:(w * h) bool) *)

(*       let quickcheck_shrinker = *)
(*         Shrinker.create (fun l -> *)
(*             Sequence.unfold ~init:l ~f:(fun l -> *)
(*                 let l' = List.map l ~f:(fun v -> if v then Random.bool () else v) in *)
(*                 if Core.not (List.exists l ~f:Fun.id) then None else Some (l', l'))) *)
(*     end) *)

let%test_unit "replicate" =
  let dx = 1 and dy = -4 and ct = 2 and l = List.init (h * w) ~f:(fun i -> i = 24) in
  let n = N.of_list l and v = V.of_list l in
  let n_ret = N.replicate ~w ~h ~dx ~dy ~ct n
  and v_ret = V.replicate ~w ~h ~dx ~dy ~ct v in
  if Base.not ([%compare.equal: Bit_list.t] (N.to_list n_ret) (V.to_list v_ret)) then (
    Caml.Format.printf "Dx=%d Dy=%d Ct=%d\nInput:\n%a\nExpected:\n%a\nGot:\n%a\n@." dx dy
      ct (N.pp_bitmap ~w) n (N.pp_bitmap ~w) n_ret (V.pp_bitmap ~w) v_ret;
    assert false)
