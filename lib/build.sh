#!/bin/sh

if uname -a | grep -q Darwin; then
    TARGETS=neon-i32x8
    OBJS=bitarray.o
else
    TARGETS=avx512skx-i32x8,avx2-i32x8,sse4-i32x8
    OBJS=bitarray.o bitarray_avx2.o bitarray_avx512skx.o bitarray_sse4.o
fi

ispc --target=$TARGETS --pic -o bitarray.o -h bitarray.h bitarray.ispc
ar -rcs libbitarray.a $OBJS
$CC -shared -o dllbitarray.so bitarray.o
