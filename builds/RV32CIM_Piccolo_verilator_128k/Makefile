###  -*-Makefile-*-

# *** DO NOT EDIT! ***
# *** This file is program-generated, not hand-written. ***
# ================================================================

REPO ?= ../..
ARCH ?= RV32CIM

TOPFILE   = $(REPO)/src_Testbench/Top/Top_FPGA_TB.bsv
TOPMODULE = mkTop_FPGA_TB

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

BSC_COMPILATION_FLAGS += \
	-D RV32 \
	-D ISA_PRIV_M  \
	-D ISA_I  -D ISA_M  -D ISA_C  \
	-D SHIFT_BARREL    \
	-D MULT_SYNTH    \
	-D Near_Mem_PT    \
	-D FABRIC64    \
	-D NO_FABRIC_BOOTROM \
	-D NO_FABRIC_PLIC \
	-D NO_FABRIC_CLINT

RAM_SIZE ?= "'h0002_0000"

# Default ISA test
TEST ?= rv32ui-p-add

# Default example
EXAMPLE = ../../sw/example/hello_world/main.elf

#================================================================
# Common boilerplate rules

include $(REPO)/builds/Resources/Include_Common.mk

#================================================================
# Makefile rules for building for specific simulator: verilator

include $(REPO)/builds/Resources/Include_verilator.mk

