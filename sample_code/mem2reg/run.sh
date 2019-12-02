#!/bin/bash

rm *.bc
rm *.ll

clang -c -O0 -emit-llvm mem2reg.c -Xclang -disable-O0-optnone
llvm-dis mem2reg.bc
opt -mem2reg -S mem2reg.ll -o mem2reg-enabled.ll

