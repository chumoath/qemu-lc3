#!/bin/sh

rm -rf build
mkdir build && cd build

../configure               \
--enable-debug             \
--enable-debug-tcg         \
--target-list=avr-softmmu

make -j6
