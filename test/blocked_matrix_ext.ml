open Base
open Base_quickcheck
include Bitarray.Blocked_matrix

let quickcheck_shrinker = Shrinker.atomic

let quickcheck_generator =
  let open Generator.Let_syntax in
  let%bind m_bit_dim = Generator.int_inclusive 1 8 in
  let round_bit_dim = Int.round_up ~to_multiple_of:8 m_bit_dim in
  let m_dim = round_bit_dim / 8 in
  let%bind m_buf = Generator.string_with_length ~length:(m_dim * m_dim * 8) in
  return (Private.unsafe_create m_buf m_dim m_bit_dim)
