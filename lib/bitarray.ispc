typedef int32 word_t;

export uniform bool bitarray_any(uniform word_t a[], uniform int len) {
  bool part = false;
  foreach (i = 0 ... len) { part |= popcnt((int32)a[i]) > 0; }
  return any(part);
}

export uniform int bitarray_hamming_weight(uniform word_t a[],
                                           uniform int len) {
  uniform int part = 0;
  for (uniform int i = 0; i < len; i++) {
    part += popcnt(a[i]);
  }
  return part;
}

inline bool read_bit(const uniform word_t x[], const int b,
                     const uniform int len) {
  int i = b >> 5;       // div by word_size = 32
  int j = b & (32 - 1); // mod by word size
  return ((x[i] >> j) & 1);
}

inline int offset(int x, int y, uniform int w, uniform int h) {
  return ((h - 1 - y) * w) + x;
}

export void bitarray_replicate(const uniform word_t a[], const uniform int dx,
                               const uniform int dy, const uniform int ct,
                               const uniform int w, const uniform int h,
                               uniform word_t r[], const uniform int len) {
  foreach (word_idx = 0 ... len) {
    int start_bit = 32 * word_idx;
    int x = start_bit % w;
    int y = h - 1 - (start_bit / w);
    word_t word = a[word_idx];
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
export void bitarray_translate(const uniform word_t a[], const uniform int dx,
                               const uniform int dy, const uniform int w,
                               const uniform int h, uniform word_t r[],
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

    word_t w1 = (read_word1 >= 0 && read_word1 < len) ? a[read_word1] : 0;
    word_t w2 = (read_word2 >= 0 && read_word2 < len) ? a[read_word2] : 0;

    int k = read_word_bit % 32;
    // take the low k bits of w1 and the high 32-k bits of w2
    r[word_idx] = (w1 << (32 - k)) | (w2 >> k);
  }
}

export void bitarray_corners(const uniform word_t a[], const uniform int w,
                             const uniform int h, uniform word_t r[],
                             const uniform int len) {
  foreach (word_idx = 0 ... len) {
    int start_bit = 32 * word_idx;
    int x = start_bit % w;
    int y = h - 1 - (start_bit / w);

    word_t word = 0;
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
