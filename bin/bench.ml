open! Core
open Core_bench
open Bitarray

let vector_sizes = [ 64; 128; 256; 512; 1024; 2048 ]

let make_unary name func =
  Bench.Test.create_indexed ~name ~args:vector_sizes (fun len ->
      let x = random len in
      Staged.stage (fun () -> func x))

let make_binary name func =
  Bench.Test.create_indexed ~name ~args:vector_sizes (fun len ->
      let x = random len in
      let x' = random len in
      Staged.stage (fun () -> func x x'))

let all = make_unary "all" all
let hash_vec = make_unary "hash-vec" hash_vec
let hash_native = make_unary "hash-native" hash
let any = make_unary "any" any
let lnot_ = make_unary "lnot" O.(lnot)
let land_ = make_binary "land" O.( land )
let lor_ = make_binary "lor" O.( lor )
let lxor_ = make_binary "lxor" O.( lxor )
let hamming_weight = make_unary "hamming-weight" hamming_weight
let hamming_distance = make_binary "hamming-distance" hamming_distance
let jaccard_distance = make_binary "jaccard-distance" jaccard_distance

let get =
  Bench.Test.create_indexed ~name:"get" ~args:vector_sizes (fun len ->
      let x = random len in
      let i = Random.int len in
      Staged.stage (fun () -> get x i))

let fold =
  Bench.Test.create_indexed ~name:"fold" ~args:vector_sizes (fun len ->
      let x = random len in
      Staged.stage (fun () -> fold ~init:() ~f:(fun _ _ -> ()) x))

let replicate =
  Bench.Test.create_with_initialization ~name:"replicate" (fun `init ->
      let x = random 256 in
      let dx = Random.int_incl (-5) 5 in
      let dy = Random.int_incl (-5) 5 in
      let ct = Random.int_incl 1 5 in
      fun () -> replicate ~w:16 ~h:16 ~dx ~dy ~ct x)

let mul =
  Bench.Test.create_indexed ~name:"mul"
    ~args:[ 1; 2; 3; 4; 5; 6; 7; 8 (* 9; 10 *) ] (fun n_blocks ->
      let x = Blocked_matrix.create (8 * n_blocks) false in
      Staged.stage (fun () -> Blocked_matrix.O.(x * x)))

let hash =
  Bench.Test.create_indexed ~name:"hash" ~args:[ 1; 2; 3; 4; 5; 6; 7; 8 ]
    (fun n_blocks ->
      let x = Blocked_matrix.create (8 * n_blocks) false in
      Staged.stage (fun () -> Blocked_matrix.O.(x * x)))

let () =
  Random.self_init ();

  (* let random_native () = Native.init ~f:(fun _ -> Random.bool ()) (64 * 4) in *)
  (* let random_vect () = Vectorized.init ~f:(fun _ -> Random.bool ()) (64 * 4) in *)

  (* let n1 = random_native () and n2 = random_native () in *)
  (* let v1 = random_vect () and v2 = random_vect () in *)
  (* let and_native = *)
  (*   Bench.Test.create ~name:"and-native" (fun () -> Native.and_ n1 n2) *)
  (* in *)
  (* let and_vect = *)
  (*   Bench.Test.create ~name:"and-vect" (fun () -> Vectorized.and_ v1 v2) *)
  (* in *)
  (* let hamming_native = *)
  (*   Bench.Test.create ~name:"native" (fun () -> Native.hamming_weight n1) *)
  (* in *)
  (* let hamming_vect = *)
  (*   Bench.Test.create ~name:"vect" (fun () -> Vectorized.hamming_weight v1) *)
  (* in *)

  (* let hamming_dist_native = *)
  (*   Bench.Test.create ~name:"native" (fun () -> Native.hamming_distance n1 n2) *)
  (* in *)
  (* let hamming_dist_vect = *)
  (*   Bench.Test.create ~name:"vect" (fun () -> Vectorized.hamming_distance v1 v2) *)
  (* in *)

  (* let rep_native = *)
  (*   Bench.Test.create ~name:"native" (fun () -> *)
  (*       Native.replicate ~w:16 ~h:16 n1 ~dx:1 ~dy:2 ~ct:5) *)
  (* in *)
  (* let rep_vect = *)
  (*   Bench.Test.create ~name:"vect" (fun () -> *)
  (*       Vectorized.replicate ~w:16 ~h:16 v1 ~dx:1 ~dy:2 ~ct:5) *)
  (* in *)

  (* let hash_native = *)
  (*   Bench.Test.create ~name:"native" (fun () -> Vectorized.hash v1) *)
  (* in *)
  (* let hash_vect = *)
  (*   Bench.Test.create ~name:"vect" (fun () -> Vectorized.vec_hash v1) *)
  (* in *)
  Command_unix.run
  @@ Command.group ~summary:"bitvector benchmarks"
       [
         ("logical", Bench.make_command [ any; all; land_; lor_; lxor_; lnot_ ]);
         ( "distance",
           Bench.make_command
             [ hamming_weight; hamming_distance; jaccard_distance ] );
         ("fold", Bench.make_command [ fold ]);
         ("get", Bench.make_command [ get ]);
         ("replicate", Bench.make_command [ replicate ]);
         ("mul", Bench.make_command [ mul ]);
         ("hash", Bench.make_command [ hash_native; hash_vec ]);
       ]
