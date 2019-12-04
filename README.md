# Programming Language Design Project (CS476)
_Instructor: William Mansky_

## Requirements

* LLVM version 9.0.1 (at minimum: clang, llvm-dis, opt)
* Python 3.7
* OCaml version 4.07.1

## Test environment

* Ubuntu Linux 18.04.1

## LLVM Optimizations implemented

* inline: Bottom-up inlining of functions into callees.
* loop-unroll: Loop unroller. 
* mem2reg: Promote Memory to Register: promotes memory references to be register references. It promotes alloca instructions which only have loads and stores as uses.

## Implementation details for the mem2reg optimization

This pass promotes memory to registers, the OCaml implementation provided as part of this project follows closely the one present in the LLVM compiler.

The general idea is the following:

1. First, we look for Alloca() instructions in the LLVM IR representation.
1. We check that the register which appears as an operand in the Alloca() doesn't appear in a Call() instruction as well, if this happens we say that the register "escapes" and it is therefore not eligible for promotion.
1. We delete the Alloca() instruction.
1. We scan the IR for corresponding Load() instructions.
1. When we find the first Load() instruction for a certain register, we scan the IR to load the src (immediate or reg) and dest of all corresponding Store() instructions.
1. We use the data grathered in the previous step to build Phi nodes and inject them instead of loads.
1. We delete all other Load()s that refer to the register.
1. We delete all Store() s that refer to the register.
1. Repeat for all registers eligible for promotion.

## Obtaining the LLVM IR from .c source code

```console
user@host:~$ clang -c -O0 -emit-llvm test.c -Xclang -disable-O0-optnone
user@host:~$ ls -l test.bc 
-rw-r--r--  1 user  group  3168 Dec  4 11:17 test.bc
```
## Disassembling the LLVM IR

```console
user@host:~$ llvm-dis test.bc
user@host:~$ ls -l test.ll 
-rw-r--r--  1 user  group  3168 Dec  4 11:18 test.ll
```

## Parsing the LLVM IR into an AST

```console
user@host:~$ python parser.py sample_code/test/test.ll
user@host:~$ ls -l out.ml 
-rw-r--r--  1 user  group  3168 Dec  4 11:19 test.ml
```
