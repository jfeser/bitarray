open Base
open Base_quickcheck

type t [@@deriving compare, equal, hash, sexp]

val hash_vec : t -> int

module O : sig
  val lnot : t -> t
  val ( lxor ) : t -> t -> t
  val ( land ) : t -> t -> t
  val ( lor ) : t -> t -> t
end

val bits_per_word : int
val nwords : int -> int
val any : t -> bool
val all : t -> bool
val is_subset : t -> of_:t -> bool
val empty : t

(** Container functions. *)

val length : t -> int
val create : int -> bool -> t
val random : int -> t
val one_hot : len:int -> int -> t
val init_fold : f:('a -> int -> 'a * bool) -> init:'a -> int -> t
val init : f:(int -> bool) -> int -> t
val get : t -> int -> bool
val set : t -> int -> bool -> t
val fold : t -> init:'a -> f:('a -> bool -> 'a) -> 'a
val iter : t -> (bool -> unit) -> unit
val iteri : t -> f:(int -> bool -> unit) -> unit

(** List conversion *)

val to_list : t -> bool list
val of_list : bool list -> t

(** Array conversion *)

val to_array : t -> bool array

(** String conversion *)

val to_string : t -> string
val of_string : int -> string -> t

(** Distance functions. *)

val hamming_weight : t -> int
val hamming_distance : t -> t -> int
val jaccard_distance : t -> t -> float

(** Bitmap functions *)

val init_bitmap : w:int -> h:int -> f:(i:int -> x:int -> y:int -> bool) -> t
val pp_bitmap : w:int -> Formatter.t -> t -> unit
val replicate : w:int -> h:int -> t -> dx:int -> dy:int -> ct:int -> t
val corners : w:int -> h:int -> t -> t

(** Boolean matrices represented using 8x8 bit blocks. *)
module Blocked_matrix : sig
  type bitarray
  type t [@@deriving compare, equal, hash, sexp, quickcheck]

  val create : int -> bool -> t
  val pp : Formatter.t -> t -> unit
  val identity : int -> t
  val upper_triangle : int -> t
  val of_matrix : bool array array -> t
  val to_matrix : t -> bool array array
  val to_bitarray : t -> bitarray
  val dim : t -> int
  val get : t -> int -> int -> bool
  val set : t -> int -> int -> bool -> t
  val pow : t -> int -> t
  val transitive_range : t -> int -> int -> t
  val iter : t -> (int * int * bool -> unit) -> unit

  (** Quickcheck functions *)

  val quickcheck_generator_with_dim : int -> t Generator.t

  module O : sig
    val ( * ) : t -> t -> t
    val lnot : t -> t
    val ( land ) : t -> t -> t
    val ( lor ) : t -> t -> t
    val ( lxor ) : t -> t -> t
  end

  module Private : sig
    val unsafe_create : string -> int -> int -> t
  end
end
with type bitarray := t

(** Short bitvectors (57 bits on 64 bit platforms) that are packed into integers. *)
module Short : sig
  type t [@@immediate] [@@deriving compare, equal, hash, sexp]

  module O : sig
    val ( land ) : t -> t -> t
    val ( lor ) : t -> t -> t
  end

  val create : int -> bool -> t
  val length : t -> int
  val get : t -> int -> bool
  val set : t -> int -> bool -> t
  val any : t -> bool
  val one_hot : len:int -> int -> t
  val to_list : t -> bool list
end
