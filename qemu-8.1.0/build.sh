#!/bin/sh

rm -rf build
mkdir build && cd build

../configure               \
--enable-debug             \
--enable-debug-tcg         \
--target-list=lc3-softmmu

# CONFIG_DEBUG_TCG

make -j6
