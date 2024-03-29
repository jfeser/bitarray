export uniform bool bitarray_any(uniform int32 a[], uniform int len) {
  bool part = false;
  foreach (i = 0 ... len) {
    part |= a[i] != 0;
  }
  return any(part);
}

export uniform bool bitarray_all(uniform int32 a[], uniform int len) {
  bool part = true;
  foreach (i = 0 ... len) {
    part &= (~a[i] == 0);
  }
  return all(part);
}

export void bitarray_and(uniform int32 a[], uniform int32 b[],
                         uniform int32 c[], uniform int len) {
  foreach (i = 0 ... len) {
    c[i] = a[i] & b[i];
  }
}

export void bitarray_or(uniform int32 a[], uniform int32 b[], uniform int32 c[],
                        uniform int len) {
  foreach (i = 0 ... len) {
    c[i] = a[i] | b[i];
  }
}

/** These functions are faster without parallelism. Benchmark before changing.
    --------------------------------------------------------------------------
 */
export uniform int bitarray_hamming_weight(uniform int32 a[], uniform int len) {
  uniform int part = 0;
  for (uniform int i = 0; i < len; i++) {
    part += popcnt(a[i]);
  }
  return part;
}

export uniform int bitarray_hamming_distance(uniform int32 a[],
                                             uniform int32 b[],
                                             uniform int len) {
  uniform int part = 0;
  for (uniform int i = 0; i < len; i++) {
    part += popcnt(a[i] ^ b[i]);
  }
  return part;
}

export uniform double bitarray_jaccard_distance(uniform int32 a[],
                                                uniform int32 b[],
                                                uniform int len) {
  if (len == 0) {
    return 0.;
  }
  uniform int inter = 0;
  uniform int union_ = 0;
  for (uniform int i = 0; i < len; i++) {
    union_ += popcnt(a[i] | b[i]);
    inter += popcnt(a[i] & b[i]);
  }
  return union_ == 0 ? 0.0 : 1.0 - ((double)inter / (double)union_);
}
/** --------------------------------------------------------------------------
 */

export uniform int bitarray_hash(uniform int8 a[], uniform int len) {
  int64 hash = 5381;
  foreach (i = 0 ... len) {
    hash = ((hash << 5) + hash) + a[i];
  }

  uniform int hashes[TARGET_WIDTH];
  hashes[programIndex] = hash;
  uniform int ret = 5381;
  for (uniform int i = 0; i < TARGET_WIDTH; i++) {
    ret = ((ret << 5) + ret) + hashes[i];
  }
  return ret;
}

inline bool read_bit(const uniform int32 x[], const int b,
                     const uniform int len) {
  int i = b >> 5;       // div by word_size = 32
  int j = b & (32 - 1); // mod by word size
  /* return ((x[i] & (1 << j))); */
  return ((x[i] >> j) & 1);
}

inline int offset(int x, int y, uniform int w, uniform int h) {
  return ((h - 1 - y) * w) + x;
}

export void bitarray_replicate(const uniform int32 a[], const uniform int dx,
                               const uniform int dy, const uniform int ct,
                               const uniform int w, const uniform int h,
                               uniform int32 r[], const uniform int len) {
  foreach (word_idx = 0 ... len) {
    int start_bit = 32 * word_idx;
    int x = start_bit % w;
    int y = h - 1 - (start_bit / w);
    int32 word = a[word_idx];
    for (uniform int b = 0; b < 32; b++) {
      bool bit = false;
      int xx = x;
      int yy = y;
      for (uniform int c = 0; c < ct && xx >= 0 && xx < w && yy >= 0 && yy < h;
           c++) {
        bit |= read_bit(a, offset(xx, yy, w, h), len);
        xx -= dx;
        yy -= dy;
      }
      word |= ((int)bit) << b;
      y = (x == w - 1) ? y - 1 : y;
      x = (x == w - 1) ? 0 : (x + 1);
    }
    r[word_idx] = word;
  }
}

// TODO: wrapping translation
export void bitarray_translate(const uniform int32 a[], const uniform int dx,
                               const uniform int dy, const uniform int w,
                               const uniform int h, uniform int32 r[],
                               const uniform int len) {
  foreach (word_idx = 0 ... len) {
    int start_bit = 32 * word_idx;
    // get the x,y coords of the least significant bit in the word
    int x = start_bit % w;
    int y = h - 1 - (start_bit / w);
    // get the shifted coords of the lsb
    int xx = x - dx;
    int yy = y - dy;
    // what is the bit offset of the shifted coords? the word that contains
    // this offset (and maybe the next word) will be the source for the
    // shift
    int read_word_bit = offset(xx, yy, w, h);
    int read_word1 = read_word_bit / 32;
    int read_word2 = read_word1 + 1;

    int32 w1 = (read_word1 >= 0 && read_word1 < len) ? a[read_word1] : 0;
    int32 w2 = (read_word2 >= 0 && read_word2 < len) ? a[read_word2] : 0;

    int k = read_word_bit % 32;
    // take the low k bits of w1 and the high 32-k bits of w2
    r[word_idx] = (w1 << (32 - k)) | (w2 >> k);
  }
}

export void bitarray_corners(const uniform int32 a[], const uniform int w,
                             const uniform int h, uniform int32 r[],
                             const uniform int len) {
  foreach (word_idx = 0 ... len) {
    int start_bit = 32 * word_idx;
    int x = start_bit % w;
    int y = h - 1 - (start_bit / w);

    int32 word = 0;
    for (uniform int b = 0; b < 32; b++) {
      int offset = start_bit + b;
      bool b1 = read_bit(a, offset - w - 1, len);
      bool b2 = read_bit(a, offset - w, len);
      bool b3 = read_bit(a, offset - w + 1, len);

      bool b4 = read_bit(a, offset - 1, len);
      bool b5 = read_bit(a, offset, len);
      bool b6 = read_bit(a, offset + 1, len);

      bool b7 = read_bit(a, offset + w - 1, len);
      bool b8 = read_bit(a, offset + w, len);
      bool b9 = read_bit(a, offset + w + 1, len);

      bool bit = x > 0 && x < w && y > 0 && y < h && b5 &&
                 (!(b1 || b2 || b4) || !(b2 || b3 || b6) || !(b4 || b7 || b8) ||
                  !(b6 || b8 || b9));

      word |= ((int)bit) << b;

      y = (x == w - 1) ? y - 1 : y;
      x = (x == w - 1) ? 0 : (x + 1);
    }
    r[word_idx] = word;
  }
}

// taken from https://stackoverflow.com/a/55307540
// CC BY-SA 4.0
uint64 bgemm8x8(uint64 A, uint64 B) {
  const uniform uint64 ROW = 0x00000000000000FF;
  const uniform uint64 COL = 0x0101010101010101;
  uint64 C = 0;
  for (uniform int i = 0; i < 8; ++i) {
    uint64 p = COL & (A >> i);
    uint64 r = ROW & (B >> i * 8);
    C |= (p * r); // use ^ for GF(2) instead
  }
  return C;
}

uniform uint64 bgemm8x8(uniform uint64 A, uniform uint64 B) {
  const uniform uint64 ROW = 0x00000000000000FF;
  const uniform uint64 COL = 0x0101010101010101;
  uniform uint64 C = 0;
  for (uniform int i = 0; i < 8; ++i) {
    uniform uint64 p = COL & (A >> i);
    uniform uint64 r = ROW & (B >> i * 8);
    C |= (p * r); // use ^ for GF(2) instead
  }
  return C;
}

#define TILE_SIZE 8

// Copyright Intel Corporation
// All rights reserved.

// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:

//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.

//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.

//     * Neither the name of Intel Corporation nor the names of its
//       contributors may be used to endorse or promote products derived from
//       this software without specific prior written permission.

// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
// This SGEMM_tileNoSIMDIntrin() code is (much!) faster than above
// implementations for serveral reasons: 1) Every memory fetch and store to
// MatrixA, MatrixB, and MatrixC is achieved w/o general gather or scatter.
//    This is because ISPC allows repeatedly changing the "axis of SPMD
//    parallelism" (e.g. multiple foreach() loops, or even nesting a foreach()
//    loop within a conventional for() loop.  Thus the compiler easily knows
//    what loop iteration variable is used to index memory, and since adjacent
//    it can easily block load adjacent elements in memory.
// 2) No atomics or horizonal intrinsics were used in this operation.
// 3) The code caches a wide sumTile[] of read/write values for re-use,
//    presumably "locally" in multiple SIMD registers.  Importantly, b.c the
//    compiler knows the single threaded foundation of the ISPC SPMD
//    programmming model, it is easy for it to enable register allocation of
//    such tiled data.
//
// But most importantly, this code is simple to undestand, debug, and easy to
// maintain.
void bgemm_64x64(uniform uint64 matrixC[], const uniform uint64 matrixA[],
                 const uniform uint64 matrixB[], uniform int N) {
  uniform uint64 sumTile[TILE_SIZE], oneAVal;

  for (uniform unsigned int m = 0; m < N; m++) {
    for (uniform unsigned int k0 = 0; k0 < N; k0 += TILE_SIZE) {
      // SPMD "horizontally" over TILE dimension:
      foreach (ki = 0 ... TILE_SIZE) {
        // No scatter required.
        sumTile[ki] = 0;
      }

      // Loop over the the matrix N dimension:
      for (uniform unsigned int n = 0; n < N; n++) {
        oneAVal = matrixA[m * N + n];
        // SPMD iterate over the TILE dimension, but within for loop nest:
        foreach (kt = 0 ... TILE_SIZE) {
          // Note, no gather required.
          varying uint64 matB = matrixB[n * N + k0 + kt];
          // Pure SIMD FMAC:
          sumTile[kt] |= bgemm8x8(oneAVal, matB);
        }
      }

      // SPMD "horizontally" again over TILE dimension:
      foreach (ki = 0 ... TILE_SIZE) {
        // Note, no scatter required.
        matrixC[m * N + k0 + ki] = sumTile[ki];
      }
    }
  }
}

export void bgemm(uniform uint64 C[], const uniform uint64 A[],
                  const uniform uint64 B[], const uniform int N) {
  if (N == 1) {
    C[0] |= bgemm8x8(A[0], B[0]);
  } else if (N == 2) {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], B[k * N + j]);
      }
    }
  } else if (N == 3) {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], B[k * N + j]);
      }
    }
  } else if (N == 4) {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], B[k * N + j]);
      }
    }
  } else if (N == 5) {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], B[k * N + j]);
      }
    }
  } else if (N == 6) {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], B[k * N + j]);
      }
    }
  } else if (N == 7) {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], B[k * N + j]);
      }
    }
  } else {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], B[k * N + j]);
      }
    }
  }
}

export void bsquare(uniform uint64 C[], const uniform uint64 A[],
                    const uniform int N) {
  if (N == 1) {
    C[0] |= bgemm8x8(A[0], A[0]);
  } else if (N == 2) {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], A[k * N + j]);
      }
    }
  } else if (N == 3) {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], A[k * N + j]);
      }
    }
  } else if (N == 4) {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], A[k * N + j]);
      }
    }
  } else if (N == 5) {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], A[k * N + j]);
      }
    }
  } else if (N == 6) {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], A[k * N + j]);
      }
    }
  } else if (N == 7) {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], A[k * N + j]);
      }
    }
  } else {
    foreach (i = 0 ... N, j = 0 ... N) {
      for (uniform int k = 0; k < N; k++) {
        C[i * N + j] |= bgemm8x8(A[i * N + k], A[k * N + j]);
      }
    }
  }
}

export void bpow(uniform uint64 B[], uniform uint64 A[], const uniform int N,
                 uniform int K) {
  const uniform int32 size = sizeof(uniform uint64) * N * N;
  assert(K > 2);
  assert(0 <= size && size <= 2 << 13);
  uniform uint64 *uniform C = (uniform uint64 * uniform) alloca(size);
  uniform uint64 *uniform P = (uniform uint64 * uniform) alloca(size);
  bool b_set = false;

  memcpy(P, A, size);
  if (K & 0x1) {
    memcpy(B, A, size);
    b_set = true;
  }
  K = K >> 1;

  while (K > 0) {
    memcpy(C, P, size);
    bsquare(P, C, N);

    if (K & 0x1) {
      if (b_set) {
        memcpy(C, B, size);
        bgemm(B, C, P, N);
      } else {
        memcpy(B, P, size);
        b_set = true;
      }
    }
    K = K >> 1;
  }
}
