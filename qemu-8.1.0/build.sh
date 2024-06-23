#!/bin/sh

rm -rf build
mkdir build && cd build

../configure               \
--enable-debug             \
--enable-debug-tcg         \
--target-list=lc3-softmmu

make -j6
