open! Base

let init ~init_fold ~f len = init_fold ~f:(fun () i -> ((), f i)) ~init:() len

let of_list ~init_fold x =
  init_fold
    ~f:(fun xs _ -> match xs with x :: xs' -> (xs', x) | [] -> assert false)
    ~init:x (List.length x)

let iteri ~fold x ~f =
  (fold x
     ~f:(fun i x ->
       f i x;
       i + 1)
     ~init:0
    : int)
  |> ignore

let init_bitmap init_fold ~w ~h ~f =
  assert (w >= 0 && h >= 0);
  let len = w * h in
  init_fold
    ~f:(fun (x, y) i ->
      let y' = if x = w - 1 then y - 1 else y in
      let x' = if x = w - 1 then 0 else x + 1 in
      ((x', y'), f ~i ~x ~y))
    ~init:(0, h - 1)
    len

let pp_bitmap iteri ~w fmt x =
  let open Caml.Format in
  iteri x ~f:(fun i b ->
      if b then fprintf fmt "█" else fprintf fmt ".";
      if i % w = w - 1 then fprintf fmt "\n")
