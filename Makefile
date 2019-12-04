#
# This is the Makefile for this project.
# You may use it to compile the sample source code
#

SAMPLE_PATH = sample_code
CLANG = clang-9
LLVMDIS=  llvm-dis-9
OPT = opt-9
PYTHON = python3.7

all: inline unroll mem2reg

mem2reg: $(SAMPLE_PATH)
	echo "Generating LLVM IR.."
	$(CLANG) -c -O0 -emit-llvm $(SAMPLE_PATH)/mem2reg/mem2reg.c -o $(SAMPLE_PATH)/mem2reg/mem2reg.bc -Xclang -disable-O0-optnone
	echo "Disassembling LLVM IR.."c
	$(LLVMDIS) $(SAMPLE_PATH)/mem2reg/mem2reg.bc -o=$(SAMPLE_PATH)/mem2reg/mem2reg.ll
	echo "Generating OPT LLVM IR.."
	$(OPT) -mem2reg -S $(SAMPLE_PATH)/mem2reg/mem2reg.ll -o $(SAMPLE_PATH)/mem2reg/mem2reg-enabled.ll
	$(PYTHON) parser.py $(SAMPLE_PATH)/mem2reg/mem2reg.ll

unroll:
	echo "Generating LLVM IR.."
	$(CLANG) -c -O0 -emit-llvm $(SAMPLE_PATH)/unroll/loop_unroll.c -o $(SAMPLE_PATH)/unroll/loop_unroll.bc -Xclang -disable-O0-optnone
	echo "Disassembling LLVM IR.."
	$(LLVMDIS) $(SAMPLE_PATH)/unroll/loop_unroll.bc -o=$(SAMPLE_PATH)/unroll/loop_unroll.ll
	echo "Generating OPT LLVM IR.."
	$(OPT) -loop-unroll -S $(SAMPLE_PATH)/unroll/loop_unroll.ll -o $(SAMPLE_PATH)/unroll/loop_unroll-enabled.ll
	$(PYTHON) parser.py $(SAMPLE_PATH)/unroll/loop_unroll.ll

inline:
	echo "Generating LLVM IR.."
	$(CLANG) -c -O0 -emit-llvm $(SAMPLE_PATH)/inline/inline.c -o $(SAMPLE_PATH)/inline/inline.bc -Xclang -disable-O0-optnone
	echo "Disassembling LLVM IR.."
	$(LLVMDIS) $(SAMPLE_PATH)/inline/inline.bc -o=$(SAMPLE_PATH)/inline/inline.ll
	echo "Generating OPT LLVM IR.."
	$(CLANG) -c -O0 -emit-llvm $(SAMPLE_PATH)/inline/inline-enabled.c -o $(SAMPLE_PATH)/inline/inline-enabled.bc -Xclang -disable-O0-optnone
	echo "Disassembling LLVM IR.."
	$(LLVMDIS) $(SAMPLE_PATH)/inline/inline-enabled.bc -o=$(SAMPLE_PATH)/inline/inline-enabled.ll
	$(PYTHON) parser.py $(SAMPLE_PATH)/inline/inline.ll

clean:
	$(RM) $(SAMPLE_PATH)/mem2reg/*.ll
	$(RM) $(SAMPLE_PATH)/mem2reg/*.bc
	$(RM) $(SAMPLE_PATH)/mem2reg/*.ml
	$(RM) $(SAMPLE_PATH)/unroll/*.ll
	$(RM) $(SAMPLE_PATH)/unroll/*.bc
	$(RM) $(SAMPLE_PATH)/unroll/*.ml
	$(RM) $(SAMPLE_PATH)/inline/*.ll
	$(RM) $(SAMPLE_PATH)/inline/*.bc
	$(RM) $(SAMPLE_PATH)/inline/*.ml
