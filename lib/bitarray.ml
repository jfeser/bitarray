module type S = sig
  type t [@@deriving compare, hash, sexp]

  val length : t -> int
  val bits_per_word : int
  val nwords : int -> int
  val create : int -> bool -> t
  val init_fold : f:('a -> int -> 'a * bool) -> init:'a -> int -> t
  val init : f:(int -> bool) -> int -> t
  val init_bitmap : w:int -> h:int -> f:(i:int -> x:int -> y:int -> bool) -> t
  val of_list : bool list -> t
  val get : t -> int -> bool
  val to_list : t -> bool list
  val fold : t -> f:('a -> bool -> 'a) -> init:'a -> 'a
  val iteri : t -> f:(int -> bool -> unit) -> unit
  val is_empty : t -> bool
  val not : t -> t
  val and_ : t -> t -> t
  val or_ : t -> t -> t
  val xor : t -> t -> t
  val hamming_weight : t -> int
  val hamming_distance : t -> t -> int
  val jaccard : t -> t -> float
  val replicate : w:int -> h:int -> t -> dx:int -> dy:int -> ct:int -> t
  val corners : w:int -> h:int -> t -> t
  val pp_bitmap : w:int -> Format.formatter -> t -> unit
end

include Vectorized
module Native = Native
module Vectorized = Vectorized
