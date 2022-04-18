open! Base
module N = Native
module V = Vectorized

let%test_unit "of_list" =
  let w = 10 and h = 10 in
  let s = List.init (w * h) ~f:(fun i -> i = 90) in
  let n = N.of_list s in
  let v = V.of_list s in
  let nl = N.to_list n and vl = V.to_list v in
  if not @@ [%compare.equal: bool list] nl vl then (
    Fmt.pr "Expected:\n%a\nActual:\n%a\n@." (N.pp_bitmap ~w) n (V.pp_bitmap ~w) v;
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
    Fmt.pr "Expected:\n%a\nActual:\n%a\n@." (N.pp_bitmap ~w) n' (V.pp_bitmap ~w) v';
    assert false)
