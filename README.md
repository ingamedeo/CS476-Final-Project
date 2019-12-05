# Programming Language Design Project (CS476)
_Instructor: William Mansky_

## Requirements

* LLVM version 9.0.1 (at minimum: clang, llvm-dis, opt)
* Python 3.7
* OCaml version 4.07.1

## Test environment

* Ubuntu Linux 18.04.1

## LLVM Optimizations implemented

* `inline`: Bottom-up inlining of functions into callees.
* `loop-unroll`: Loop unroller. 
* `mem2reg`: Promote Memory to Register: promotes memory references to be register references. It promotes alloca instructions which only have loads and stores as uses.

## Implementation details for the `inline` optimization
This pass inlines function calls in the `main` function body.
It does this in 2 macro passes: 
1. Function Inline
    1. Looks for function calls in `main` and, if found, look if they are present in the function table (i.e. are declared and non library functions).
    2. Looks for the _return_ register (either operand or return value) of the function and uses that register number in order to determine the base of the offset in order to later rename the registers.
    3. Takes the body of the called function and finds the first and last register used to determine the offset in order to later rename the registers.
    4. Renames registers of the body of the function according to the base and the offset.
    5. Converts to `Nop`s every `Load` and `Store` which uses registers whose values are less than the base offset (that is, those are the `Load`s and `Store`s that use the input parameters of the function).
    6. Inlines (copies) the resulting body in the `main` function.
    7 Removes `Return` instruction and substitutes it with a `Load` into the output register (if any).
    8. Offsets all the subsequent registers in `main` by the addres of the last used register in the inlined function (minus the base offset).
    
2. Cleanup
    1. Removes all the added `Nop`s.
    2. Readds `Return`s if deleted by the previous pass.

## Implementation details for the `unroll` optimization
This pass unrolls the loop with a given label by a constant factor, specified as the (immediate) argument of the first `Icmp` after the label.

1. Loop Identification
    1. Builds a `label_table` in which all labels are mapped to the the set of instructions that follows it until a branch pointing to that label is identified.
    2. Finds the unroll factor of that loop by looking to the nearest `Icmp` instruction.
    
2. Loop Unrolling 
    1. Generates a new body of the loop in which the loop is repeated as specified by the unroll factor.
    2. Renames registers in the loop bodies according to an offset determined by how many times the loop needs to be unrolled.
    3. Removes now useless `Icmp` instructions.
    4. Injects the new loop body at the label found before.
    5. Shifts all subsequent instructions by the unroll factor times the loop body length.

## Implementation details for the `mem2reg` optimization

This pass promotes memory to registers, the OCaml implementation provided as part of this project follows closely the one present in the LLVM compiler.

The general idea is the following:

1. First, we look for `Alloca` instructions in the LLVM IR representation.
1. We check that the register which appears as an operand in the `Alloca` doesn't appear in a `Call` instruction as well, if this happens we say that the register "escapes" and it is therefore not eligible for promotion.
1. We delete the `Alloca` instruction.
1. We scan the IR for corresponding `Load` instructions.
1. When we find the first `Load` instruction for a certain register, we scan the IR to load the src (immediate or reg) and dest of all corresponding `Store` instructions.
1. We use the data gathered in the previous step to build Phi nodes and inject them instead of loads.
1. We delete all other `Load`s that refer to the register.
1. We delete all `Store`s that refer to the register.
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

## Generating the `AST`s from the source code.
```console
user@host:~$ make [all | mem2reg | unroll | inline | clean]
```
This generates the `.ml` `AST` files together with all the instruction types.

The `opts` folder contains the LLVM optimizations implemented in OCaml.