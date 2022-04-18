open! Core
open Core_bench
open Bitarray

let () =
  Random.self_init ();
  let random_native () = Native.init ~f:(fun _ -> Random.bool ()) (64 * 4) in
  let random_vect () = Vectorized.init ~f:(fun _ -> Random.bool ()) (64 * 4) in

  let n1 = random_native () and n2 = random_native () in
  let v1 = random_vect () and v2 = random_vect () in
  let and_native = Bench.Test.create ~name:"and-native" (fun () -> Native.and_ n1 n2) in
  let and_vect = Bench.Test.create ~name:"and-vect" (fun () -> Vectorized.and_ v1 v2) in
  let hamming_native =
    Bench.Test.create ~name:"native" (fun () -> Native.hamming_weight n1)
  in
  let hamming_vect =
    Bench.Test.create ~name:"vect" (fun () -> Vectorized.hamming_weight v1)
  in

  let hamming_dist_native =
    Bench.Test.create ~name:"native" (fun () -> Native.hamming_distance n1 n2)
  in
  let hamming_dist_vect =
    Bench.Test.create ~name:"vect" (fun () -> Vectorized.hamming_distance v1 v2)
  in

  let rep_native =
    Bench.Test.create ~name:"native" (fun () ->
        Native.replicate ~w:16 ~h:16 n1 ~dx:1 ~dy:2 ~ct:5)
  in
  let rep_vect =
    Bench.Test.create ~name:"vect" (fun () ->
        Vectorized.replicate ~w:16 ~h:16 v1 ~dx:1 ~dy:2 ~ct:5)
  in

  let hash_native = Bench.Test.create ~name:"native" (fun () -> Vectorized.hash v1) in
  let hash_vect = Bench.Test.create ~name:"vect" (fun () -> Vectorized.vec_hash v1) in

  Command_unix.run
  @@ Command.group ~summary:"bitvector benchmarks"
       [
         ("hash", Bench.make_command [ hash_native; hash_vect ]);
         ( "other",
           Bench.make_command
             [
               Bench.Test.create_group ~name:"replicate" [ rep_vect; rep_native ];
               Bench.Test.create_group ~name:"and" [ and_native; and_vect ];
               Bench.Test.create_group ~name:"hamming-weight"
                 [ hamming_native; hamming_vect ];
               Bench.Test.create_group ~name:"hamming-dist"
                 [ hamming_dist_native; hamming_dist_vect ];
             ] );
       ]
