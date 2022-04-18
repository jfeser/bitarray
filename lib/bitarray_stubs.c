#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdbool.h>
#include <stdio.h>

#include "bitarray.h"

typedef int32_t word_t;

#define len(s) (caml_string_length(s) / (sizeof(word_t)))

CAMLprim value bitarray_and_stub(value b1, value b2, value b3) {
  const word_t *p1 = (const word_t *)String_val(b1),
               *p2 = (const word_t *)String_val(b2);
  word_t *p3 = (word_t *)Bytes_val(b3);

  for (int i = 0; i < len(b1); i++) {
    p3[i] = p1[i] & p2[i];
  }
  return Val_unit;
}

CAMLprim value bitarray_or_stub(value b1, value b2, value b3) {
  const word_t *p1 = (const word_t *)String_val(b1),
               *p2 = (const word_t *)String_val(b2);
  word_t *p3 = (word_t *)Bytes_val(b3);

  for (int i = 0; i < len(b1); i++) {
    p3[i] = p1[i] | p2[i];
  }
  return Val_unit;
}

CAMLprim value bitarray_xor_stub(value b1, value b2, value b3) {
  const word_t *p1 = (const word_t *)String_val(b1),
               *p2 = (const word_t *)String_val(b2);
  word_t *p3 = (word_t *)Bytes_val(b3);

  for (int i = 0; i < len(b1); i++) {
    p3[i] = p1[i] ^ p2[i];
  }
  return Val_unit;
}

CAMLprim value bitarray_any_stub(value b) {
  return Val_bool(bitarray_any((word_t *)(String_val(b)), len(b)));
}

CAMLprim value bitarray_not_stub(value b1, value b2) {
  const word_t *p1 = (const word_t *)String_val(b1);
  word_t *p2 = (word_t *)Bytes_val(b2);

  for (int i = 0; i < len(b1); i++) {
    p2[i] = ~p1[i];
  }
  return Val_unit;
}

CAMLprim intnat bitarray_hamming_weight_stub(value b) {
  return bitarray_hamming_weight((word_t *)(String_val(b)), len(b));
}

CAMLprim value bitarray_hamming_weight_stub_byte(value b) {
  return Val_int(bitarray_hamming_weight_stub(b));
}

CAMLprim intnat bitarray_hamming_distance_stub(value b1, value b2) {
  const word_t *p1 = (const word_t *)String_val(b1),
               *p2 = (const word_t *)String_val(b2);
  int count = 0;

#pragma GCC unroll 8
  for (int i = 0; i < len(b1); i++) {
    count += __builtin_popcount(p1[i] ^ p2[i]);
  }
  return count;
}

CAMLprim value bitarray_hamming_distance_stub_byte(value b1, value b2) {
  return Val_int(bitarray_hamming_distance_stub(b1, b2));
}

CAMLprim double bitarray_jaccard_stub(value b1, value b2) {
  const word_t *p1 = (const word_t *)String_val(b1),
               *p2 = (const word_t *)String_val(b2);
  int union_ = 0, inter = 0, len = len(b1);

  if (len == 0) {
    return 0.;
  }
  for (int i = 0; i < len; i++) {
    union_ += __builtin_popcount(p1[i] | p2[i]);
    inter += __builtin_popcount(p1[i] & p2[i]);
  }

  return union_ == 0 ? 0.0 : 1.0 - ((double)inter / (double)union_);
}

CAMLprim value bitarray_jaccard_stub_byte(value b1, value b2) {
  return caml_copy_double(bitarray_jaccard_stub(b1, b2));
}

CAMLprim value bitarray_replicate_stub(value b1, intnat x, intnat y, intnat ct,
                                       intnat w, intnat h, value b2) {
  bitarray_replicate((word_t *)(String_val(b1)), x, y, ct, w, h,
                     (word_t *)(Bytes_val(b2)), len(b1));
  return Val_unit;
}

CAMLprim value bitarray_replicate_stub_byte(value b1, value x, value y,
                                            value ct, value w, value h,
                                            value b2) {
  return bitarray_replicate_stub(b1, Int_val(x), Int_val(y), Int_val(ct),
                                 Int_val(w), Int_val(h), b2);
}

CAMLprim intnat bitarray_hash_stub(value d, value k) {
  int *dd = (int *)d;
  int *kk = (int *)k;
  long sum = 0;
  for (int i = 0; i < len(d); i += 2) {
    sum += (long)(dd[i] + kk[i]) * (long)(dd[i + 1] + kk[i + 1]);
  }
  return sum;
}

CAMLprim value bitarray_hash_stub_byte(value d, value k) {
  return Val_int(bitarray_hash_stub(d, k));
}

CAMLprim value bitarray_corners_stub(value b1, intnat w, intnat h, value b2) {
  bitarray_corners((word_t *)(String_val(b1)), w, h, (word_t *)(Bytes_val(b2)),
                   len(b1));
  return Val_unit;
}

CAMLprim value bitarray_corners_stub_byte(value b1, value w, value h,
                                          value b2) {
  return bitarray_corners_stub(b1, Int_val(w), Int_val(h), b2);
}
