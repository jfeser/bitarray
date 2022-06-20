open Base

module type S = sig
  type t [@@deriving compare, hash, sexp]

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

  (** Container functions. *)

  val length : t -> int
  val create : int -> bool -> t
  val random : int -> t
  val init_fold : f:('a -> int -> 'a * bool) -> init:'a -> int -> t
  val init : f:(int -> bool) -> int -> t
  val get : t -> int -> bool
  val fold : t -> init:'a -> f:('a -> bool -> 'a) -> 'a
  val iteri : t -> f:(int -> bool -> unit) -> unit

  (** List conversion *)

  val to_list : t -> bool list
  val of_list : bool list -> t

  (** Array conversion *)

  val to_array : t -> bool array

  (** Distance functions. *)

  val hamming_weight : t -> int
  val hamming_distance : t -> t -> int
  val jaccard_distance : t -> t -> float

  (** Bitmap functions *)

  val init_bitmap : w:int -> h:int -> f:(i:int -> x:int -> y:int -> bool) -> t
  val pp_bitmap : w:int -> Formatter.t -> t -> unit
  val replicate : w:int -> h:int -> t -> dx:int -> dy:int -> ct:int -> t
  val corners : w:int -> h:int -> t -> t

  module Blocked_matrix : sig
    type t [@@deriving compare, hash, sexp, quickcheck]

    val create : int -> bool -> t
    val to_matrix : t -> bool array array

    module O : sig
      val ( * ) : t -> t -> t
      val lnot : t -> t
      val ( land ) : t -> t -> t
      val ( lor ) : t -> t -> t
      val ( lxor ) : t -> t -> t
    end
  end
end
