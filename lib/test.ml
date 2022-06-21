open Base
open Base_quickcheck
module N = Native
module V = Vectorized

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

module Bit_matrix = struct
  type nonrec t = bool array array [@@deriving compare, quickcheck, sexp]

  let quickcheck_shrinker = Shrinker.atomic

  let random dim =
    Array.init dim ~f:(fun _ -> Array.init dim ~f:(fun _ -> Random.bool ()))

  let quickcheck_generator =
    let open Generator.Let_syntax in
    let%bind dim = Generator.small_strictly_positive_int in
    return (random dim)

  let pp fmt x =
    let n = Array.length x in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if x.(i).(j) then Fmt.pf fmt "█" else Fmt.pf fmt "."
      done;
      Fmt.pf fmt "\n"
    done

  let ( * ) a b =
    let n = Array.length a in
    let c = Array.make_matrix ~dimx:n ~dimy:n false in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        for k = 0 to n - 1 do
          c.(i).(j) <- c.(i).(j) || (a.(i).(k) && b.(k).(j))
        done
      done
    done;
    c
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
      [%test_result: Bit_list.t]
        ~expect:(N.to_list @@ fn n n')
        (V.to_list @@ fv v v'))
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

let%test_unit "and" = make_binary_test N.and_ V.O.( land )
let%test_unit "or" = make_binary_test N.or_ V.O.( lor )
let%test_unit "xor" = make_binary_test N.xor V.O.( lxor )
let%test_unit "not" = make_unary_test N.not V.O.(lnot)
let%test_unit "jaccard" = make_binary_float_test N.jaccard V.jaccard_distance

let%test_unit "get" =
  Test.run_exn
    ~f:(fun a ->
      let n = N.of_list a
      and v = V.of_list a
      and idx = Random.int (List.length a) in
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
      [%test_result: int] ~expect:(N.hamming_distance n n')
        (V.hamming_distance v v'))
    (module Bit_list_equal_len_pair)

let w = 16
and h = 16

let%test_unit "replicate" =
  let dx = 1
  and dy = -4
  and ct = 2
  and l = List.init (h * w) ~f:(fun i -> i = 24) in
  let n = N.of_list l and v = V.of_list l in
  let n_ret = N.replicate ~w ~h ~dx ~dy ~ct n
  and v_ret = V.replicate ~w ~h ~dx ~dy ~ct v in
  if Base.not ([%compare.equal: Bit_list.t] (N.to_list n_ret) (V.to_list v_ret))
  then (
    Caml.Format.printf
      "Dx=%d Dy=%d Ct=%d\nInput:\n%a\nExpected:\n%a\nGot:\n%a\n@." dx dy ct
      (N.pp_bitmap ~w) n (N.pp_bitmap ~w) n_ret (V.pp_bitmap ~w) v_ret;
    assert false)

let%test_unit "of_list" =
  let w = 10 and h = 10 in
  let s = List.init (w * h) ~f:(fun i -> i = 90) in
  let n = N.of_list s in
  let v = V.of_list s in
  let nl = N.to_list n and vl = V.to_list v in
  if not @@ [%compare.equal: bool list] nl vl then (
    Fmt.pr "Expected:\n%a\nActual:\n%a\n@." (N.pp_bitmap ~w) n (V.pp_bitmap ~w)
      v;
    assert false)

let%test_unit "replicate" =
  let w = 10 and h = 10 and dx = 0 and dy = 2 and ct = 5 in
  let s = List.init (w * h) ~f:(fun i -> i = 90) in
  let n = N.of_list s in
  let v = V.of_list s in
  let n' = N.replicate ~w ~h n ~dx ~dy ~ct in
  let v' = V.replicate ~w ~h v ~dx ~dy ~ct in
  let nl = N.to_list n' and vl = V.to_list v' in
  if not @@ [%compare.equal: bool list] nl vl then (
    Fmt.pr "Expected:\n%a\nActual:\n%a\n@." (N.pp_bitmap ~w) n' (V.pp_bitmap ~w)
      v';
    assert false)

let%test_unit "mul" =
  let module VM = V.Blocked_matrix in
  Test.run_exn
    ~f:(fun m ->
      let expect = Bit_matrix.(VM.to_matrix m * VM.to_matrix m) in
      let actual = V.Blocked_matrix.(to_matrix O.(m * m)) in
      [%test_result: Bit_matrix.t] ~expect actual)
    (module V.Blocked_matrix)

let%test_unit "mul" =
  let module VM = V.Blocked_matrix in
  let a =
    VM.of_matrix
      [|
        [| false; true; false; false; false |];
        [| false; false; true; false; false |];
        [| false; false; false; true; false |];
        [| false; false; false; false; false |];
        [| false; false; false; false; false |];
      |]
  in
  let b =
    VM.of_matrix
      [|
        [| false; false; false; false; false |];
        [| false; false; false; false; false |];
        [| false; false; false; false; false |];
        [| false; false; false; false; true |];
        [| false; false; false; false; false |];
      |]
  in
  Stdio.print_s [%message (VM.to_matrix VM.O.(a * a * b) : Bit_matrix.t)]

let%expect_test "to_matrix" =
  let module VM = V.Blocked_matrix in
  let zero =
    VM.{ m_buf = "\x00\x00\x00\x00\x00\x00\x00\x00"; m_dim = 1; m_bit_dim = 8 }
  in
  Fmt.pr "%a\n" Bit_matrix.pp (VM.to_matrix zero);
  let id =
    VM.
      {
        m_buf = String.rev "\x80\x40\x20\x10\x08\x04\x02\x01";
        m_dim = 1;
        m_bit_dim = 8;
      }
  in
  let id' = VM.identity 8 in
  Fmt.pr "%a\n" Bit_matrix.pp (VM.to_matrix id);
  Fmt.pr "%a\n" Bit_matrix.pp (VM.to_matrix id');
  Fmt.pr "%a\n" Bit_matrix.pp VM.(to_matrix O.(id * id));
  [%expect
    {|
    ........
    ........
    ........
    ........
    ........
    ........
    ........
    ........

    █.......
    .█......
    ..█.....
    ...█....
    ....█...
    .....█..
    ......█.
    .......█

    █.......
    .█......
    ..█.....
    ...█....
    ....█...
    .....█..
    ......█.
    .......█

    █.......
    .█......
    ..█.....
    ...█....
    ....█...
    .....█..
    ......█.
    .......█ |}]
