###  -*-Makefile-*-

# Copyright (c) 2018 Bluespec, Inc. All Rights Reserved

# ================================================================

.PHONY: help
help:
	@echo 'options: sim32 b32 sim64 b64 simV64 bV64 simD64 bD64 test'

.PHONY: all
all: compile link

.PHONY: trueall
trueall: sim32 b32 sim64 b64 simV64 bV64 simD64 bD64

# ================================================================
# Compiling the system

# ----------------
# Search path for bsc for .bsv files

CORE_DIRS = $(REPO)/src_Core/ISA:$(REPO)/src_Core/RegFiles:$(REPO)/src_Core/Core:$(REPO)/src_Core/Near_Mem_VM:$(REPO)/src_Core/BSV_Additional_Libs:$(REPO)/src_Verifier
TESTBENCH_DIRS  = $(REPO)/src_Testbench/Top:$(REPO)/src_Testbench/SoC:$(REPO)/src_Testbench/Fabrics/AXI4_Lite
BSC_PATH = -p $(CORE_DIRS):$(TESTBENCH_DIRS):+
USEFUL_FLAGS = #-remove-unused-modules -remove-empty-rules -remove-starved-rules -remove-false-rules

# ----------------
# Top-level file and module

TOPFILE   = $(REPO)/src_Testbench/Top/Top_HW_Side.bsv
TOPMODULE = mkTop_HW_Side

#----------------
# bsc flags

# Implementation choice: SHIFT_BARREL, SHIFT_SERIAL, SHIFT_MULT, SHIFT_NONE
# Implementation choice: MULT_SYNTH, MULT_SERIAL

BSC_FLAGS += -keep-fires -aggressive-conditions -no-warn-action-shadowing \
		-suppress-warnings G0020 \
		-D RV32 \
		-D FABRIC32 \
		-D ISA_PRIV_M \
		-D ISA_PRIV_U \
		-D ISA_M \
		-D Near_Mem_Caches \
		-D SHIFT_BARREL \
		-D CSR_REGFILE_UM \
		+RTS -K64M -RTS  -show-range-conflict
		
BSC_FLAGS64 += -keep-fires -aggressive-conditions -no-warn-action-shadowing \
		-suppress-warnings G0020 \
		-D RV64 \
		-D FABRIC64 \
		-D SV39 \
		-D ISA_PRIV_M \
		-D ISA_PRIV_U \
		-D ISA_M \
		-D Near_Mem_Caches \
		-D SHIFT_BARREL \
		-D CSR_REGFILE_UM \
		+RTS -K64M -RTS  -show-range-conflict

BSC_FLAGS64FULL += -keep-fires -aggressive-conditions -no-warn-action-shadowing \
		-suppress-warnings G0020 \
		-D RV64 \
		-D FABRIC64 \
		-D SV39 \
		-D ISA_PRIV_M \
		-D ISA_PRIV_U \
		-D ISA_PRIV_S \
		-D ISA_M \
		-D ISA_A \
		-D Near_Mem_Caches \
		-D SHIFT_BARREL \
		-D CSR_REGFILE_UM \
		+RTS -K64M -RTS  -show-range-conflict
		
BSC_FLAGS64VERIF += -keep-fires -aggressive-conditions -no-warn-action-shadowing \
		-suppress-warnings G0020 \
		-D RV64 \
		-D FABRIC64 \
		-D SV39 \
		-D ISA_PRIV_M \
		-D ISA_PRIV_U \
		-D ISA_PRIV_S \
		-D ISA_M \
		-D ISA_A \
		-D Near_Mem_Caches \
		-D SHIFT_BARREL \
		-D CSR_REGFILE_UM \
		-D INCLUDE_WOLF_VERIF \
		+RTS -K64M -RTS  -show-range-conflict

BSC_FLAGSGDB += -keep-fires -aggressive-conditions -no-warn-action-shadowing \
		-suppress-warnings G0020 \
		-D RV64 \
		-D FABRIC64 \
		-D SV39 \
		-D ISA_PRIV_M \
		-D ISA_PRIV_U \
		-D ISA_M \
		-D INCLUDE_GDB_CONTROL\
		-D Near_Mem_Caches \
		-D FABRIC64 \
		+RTS -K64M -RTS  -show-range-conflict

# ================================================================
# Make targets

TMP_DIRS  = -bdir build  -simdir build  -info-dir build
TMP_VDIRS = -bdir buildv -simdir buildv -info-dir buildv -vdir buildv
TMP_SDIRS = -bdir buildsoc -simdir buildsoc -info-dir buildsoc -vdir buildsoc
TMP_64DIRS = -bdir build64 -simdir build64 -info-dir build64 -vdir build64soc
TMP_64FDIRS = -bdir buildF64 -simdir buildF64 -info-dir buildF64 -vdir buildF64
TMP_64SIM = -bdir sim64 -simdir sim64 -info-dir sim64 -vdir sim64
TMP_VERIF = -bdir verifsim -simdir verifsim -info-dir verifsim -vdir verifsim
TMP_LOGIF = -bdir logif -simdir logif -info-dir logif -vdir logif
TMP_GDBV = -bdir gdbv -simdir gdbv -info-dir gdbv -vdir gdbv
TMP_GDB = -bdir gdb -simdir gdb -info-dir gdb -vdir gdb

.PHONY: sim32
compile:
	@echo "INFO: Compiling simulation (CPU, 32-bit)"
	mkdir -p  build
	bsc -u -elab -sim -D SIM $(USEFUL_FLAGS) $(TMP_DIRS)  $(BSC_FLAGS)  $(BSC_PATH)  $(TOPFILE)
	@echo "INFO: Compiled  Core (CPU, Caches)"
	@echo "INFO: Linking bsc-compiled objects into Bluesim executable"
	bsc -sim -parallel-sim-link 8 \
		$(TMP_DIRS)\
		-e $(TOPMODULE) -o ./$(BSIM_EXE_FILE) \
		$(BSC_C_FLAGS)
	@echo "INFO: Linked bsc-compiled objects into Bluesim executable"

.PHONY: b32
verilog:
	@echo "INFO: Compiling to verilog (CPU, 32-bit)"
	mkdir -p  buildv
	bsc -u -elab -verilog -D VERILOG $(USEFUL_FLAGS) $(TMP_VDIRS)  $(BSC_FLAGS)  $(BSC_PATH)  ../src_Core/Core/CPU.bsv
	@echo "INFO: Re-compiled  Core (CPU)"

.PHONY: sim64
sim64:
	@echo "INFO: Compiling simulation (CPU, 64-bit)"
	mkdir -p  sim64
	bsc -u -elab -sim -D SIM $(USEFUL_FLAGS) $(TMP_64SIM)  $(BSC_FLAGS64FULL)  $(BSC_PATH)  $(TOPFILE)
	@echo "INFO: Re-compiled  Core (CPU, Caches)"
	@echo "INFO: linking bsc-compiled objects into Bluesim executable"
	bsc -sim -parallel-sim-link 8 \
		$(TMP_64SIM)\
		-e $(TOPMODULE) -o ./$(BSIM_EXE_FILE64) \
		$(BSC_C_FLAGS)
	@echo "INFO: linked bsc-compiled objects into Bluesim executable"

.PHONY: b64
b64:
	@echo "INFO: Compiling to verilog (CPU, 64-bit)"
	mkdir -p build64
	bsc -u -elab -verilog -D VERILOG $(USEFUL_FLAGS) $(TMP_64DIRS) $(BSC_FLAGS64) $(BSC_PATH) ../src_Core/Core/CPU.bsv
	@echo "INFO: Compiled 64-bit CPU to verilog"
	
.PHONY: bF64
bF64:
	@echo "INFO: Compiling experimental ISA_FD verilog (CPU, 64-bit)"
	mkdir -p buildF64
	bsc -u -elab -verilog -D VERILOG $(USEFUL_FLAGS) $(TMP_64FDIRS) $(BSC_FLAGS64) -D ISA_F -D ISA_D $(BSC_PATH) ../src_Core/Core/CPU.bsv
	@echo "INFO: Compiled 64-bit CPU to verilog"

.PHONY: simV64
simV64:
	@echo "INFO: Compiling CPU with Verifier attachments"
	mkdir -p  verifsim
	bsc -u -elab -sim -D SIM $(TMP_VERIF) $(BSC_FLAGS64VERIF)  $(BSC_PATH)  $(TOPFILE)
	@echo "INFO: Compilation completed"
	@echo "INFO: linking bsc-compiled objects into Bluesim executable"
	bsc -sim -parallel-sim-link 8 \
		$(TMP_VERIF) \
		-e $(TOPMODULE) -o ./$(BSIM_EXE_FV64) \
		$(BSC_C_FLAGS)
	@echo "INFO: linked bsc-compiled objects into Bluesim executable"

.PHONY: bV64
bV64:
	@echo "INFO: Compiling Verifier CPU to verilog"
	mkdir -p logif
	bsc -u -elab -verilog -D VERILOG $(TMP_LOGIF) $(BSC_FLAGS64VERIF)  $(BSC_PATH) -g mkVerifier_CPU ../src_Verifier/Verifier_CPU.bsv
	@echo "INFO: Compilation completed"


.PHONY: simD64
simD64:
	@echo "INFO: Compiling GDB CPU for simulation"
	mkdir -p gdb
	bsc -u -elab -sim -D SIM $(USEFUL_FLAGS) $(TMP_GDB) $(BSC_FLAGSGDB) $(BSC_PATH) $(TOPFILE)
	@echo "INFO: Compilation completed, starting linking"
	bsc -sim -parallel-sim-link 8 \
		$(TMP_GDB)\
		-e $(TOPMODULE) -o ./$(BSIM_EXE_FILEGDB) \
		$(BSC_C_FLAGS)
    
    
.PHONY: bD64
bD64:
	@echo "INFO: Compiling GDB CPU to verilog"
	mkdir -p gdbv
	bsc -u -elab -verilog $(USEFUL_FLAGS) $(TMP_GDBV) $(BSC_FLAGSGDB) $(BSC_PATH) ../src_Core/Core/CPU.bsv
	@echo "INFO: Compilation completed"


BSIM_EXE_FILE = exe_HW_bsim
BSIM_EXE_FILE64 = exe_HW_bsim64
BSIM_EXE_FV64 = exe_HW_vs64
BSIM_EXE_FILEGDB = exe_GDB

BSC_C_FLAGS += \
	-Xc++  -D_GLIBCXX_USE_CXX11_ABI=0 \
	-Xl -v \
	-Xc -O3 -Xc++ -O3 \

# ================================================================
# Test: run the executable on the standard 'add' RISCV ISA test

.PHONY: test
test:
	make -C  ../Tests/elf_to_hex
	../Tests/elf_to_hex/elf_to_hex  ../Tests/isa/rv32ui-p-sub  Mem.hex
	./$(BSIM_EXE_FILE)  -m 2500  +v1  +tohost

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~  build
	rm -rf buildv
	rm -rf buildsoc
	
.PHONY: simclean
simclean:
	rm -rf build

.PHONY: vclean
vclean:
	rm -rf buildv
	rm -rf buildsoc

.PHONY: full_clean
full_clean: clean
	rm -r -f  *~  $(SIM_EXE_FILE)*  *.log  *.vcd  *.hex

# ================================================================
