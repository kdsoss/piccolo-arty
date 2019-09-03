###  -*-Makefile-*-

# *** DO NOT EDIT! ***
# *** This file is program-generated, not hand-written. ***
# ================================================================

REPO ?= ../..
ARCH ?= RV64ACIMU

TOPMODULE ?= $(TOPMODULE_NORMAL)  \

CAPSIZE = 128
TAGS_STRUCT = 0 128
TAGS_ALIGN = 32

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler


BSC_COMPILATION_FLAGS += \
	-D RV64 \
	-D ISA_PRIV_M -D ISA_PRIV_U  \
        -D RISCV \
	-D CAP128 \
	-D MEM128 \
	-D ISA_I  -D ISA_M -D ISA_C -D ISA_A  -D ISA_CHERI \
	-D SHIFT_BARREL    \
	-D MULT_SYNTH    \
	-D Near_Mem_Caches    \
	-D FABRIC64 \
	-D INCLUDE_GDB_CONTROL \
	-D BRVF_TRACE \
	-D XILINX_BSCAN  -D XILINX_XCVU9P  -D JTAG_TAP

# Default ISA test

TEST ?= rv32ui-p-add

#================================================================
# Common boilerplate rules

include $(REPO)/builds/Resources/Include_Common.mk

#================================================================
# Makefile rules for building for specific simulator: bluesim

include $(REPO)/builds/Resources/Include_bluesim.mk


