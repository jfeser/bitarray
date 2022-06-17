module type S = Bitarray_intf.S

include S
module Vectorized : S
