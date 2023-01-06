#################################################################################################
# << Piccolo - Application Makefile >>                                                          #
# Modified (by Ashwin Menon) from the NEORV32 Application Makefile                              #
#   - The NEORV32 Processor - https://github.com/stnolting/neorv32          (c) Stephan Nolting #
# ********************************************************************************************* #
# BSD 3-Clause License                                                                          #
#                                                                                               #
# Copyright (c) 2022, Stephan Nolting. All rights reserved.                                     #
# Copyright (c) 2022, Ashwin Menon. All rights reserved.                                        #
#                                                                                               #
# Redistribution and use in source and binary forms, with or without modification, are          #
# permitted provided that the following conditions are met:                                     #
#                                                                                               #
# 1. Redistributions of source code must retain the above copyright notice, this list of        #
#    conditions and the following disclaimer.                                                   #
#                                                                                               #
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of     #
#    conditions and the following disclaimer in the documentation and/or other materials        #
#    provided with the distribution.                                                            #
#                                                                                               #
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to  #
#    endorse or promote products derived from this software without specific prior written      #
#    permission.                                                                                #
#                                                                                               #
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS   #
# OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF               #
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE    #
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,     #
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE #
# GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED    #
# AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     #
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED  #
# OF THE POSSIBILITY OF SUCH DAMAGE.                                                            #
# ********************************************************************************************* #
# The Piccolo Processor - https://github.com/bluespec/Piccolo                      (c) Bluespec #
#################################################################################################


# -----------------------------------------------------------------------------
# USER CONFIGURATION
# -----------------------------------------------------------------------------
# User's application sources (*.c, *.cpp, *.s, *.S); add additional files here
APP_SRC ?= $(wildcard ./*.c) $(wildcard ./*.s) $(wildcard ./*.cpp) $(wildcard ./*.S)

# User's application include folders (don't forget the '-I' before each entry)
APP_INC ?= -I .
# User's application include folders - for assembly files only (don't forget the '-I' before each entry)
ASM_INC ?= -I .

# Optimization
EFFORT ?= -Os

# Compiler toolchain
RISCV_PREFIX ?= riscv32-unknown-elf-

# CPU architecture and ABI
MARCH ?= rv32i
MABI  ?= ilp32

# User flags for additional configuration (will be added to compiler flags)
USER_FLAGS ?=

# Relative or absolute path to the Piccolo-SW home folder
PICCOLO_SW_HOME ?= ../


# -----------------------------------------------------------------------------
# Piccolo-SW framework
# -----------------------------------------------------------------------------
# Path to Piccolo-SW linker script and startup file
PICCOLO_SW_COM_PATH = $(PICCOLO_SW_HOME)/common
# Path to main Piccolo-SW library include files
PICCOLO_SW_INC_PATH = $(PICCOLO_SW_HOME)/lib/include
# Path to main Piccolo-SW library source files
PICCOLO_SW_SRC_PATH = $(PICCOLO_SW_HOME)/lib/source
# Path to Piccolo-SW executable generator
PICCOLO_SW_EXG_PATH = $(PICCOLO_SW_HOME)/image_gen

# Core libraries (peripheral and CPU drivers)
CORE_SRC  = $(wildcard $(PICCOLO_SW_SRC_PATH)/*.c)
# Application start-up code
CORE_SRC += $(PICCOLO_SW_COM_PATH)/crt0.S

# Linker script
LD_SCRIPT = $(PICCOLO_SW_COM_PATH)/piccolo.ld

# Main output files
APP_EXE  = piccolo_exe.bin
APP_ELF  = main.elf
APP_HEX  = piccolo_raw_exe.hex
APP_BIN  = piccolo_raw_exe.bin
APP_ASM  = main.asm


# -----------------------------------------------------------------------------
# Sources and objects
# -----------------------------------------------------------------------------
# Define all sources
SRC  = $(APP_SRC)
SRC += $(CORE_SRC)

# Define all object files
OBJ = $(SRC:%=%.o)


# -----------------------------------------------------------------------------
# Tools and flags
# -----------------------------------------------------------------------------
# Compiler tools
CC      = $(RISCV_PREFIX)gcc
OBJDUMP = $(RISCV_PREFIX)objdump
OBJCOPY = $(RISCV_PREFIX)objcopy
SIZE    = $(RISCV_PREFIX)size

# Host native compiler
CC_X86 = gcc -Wall -O -g

# Piccolo executable image generator
IMAGE_GEN = $(PICCOLO_SW_EXG_PATH)/image_gen

# Compiler & linker flags
CC_OPTS  = -march=$(MARCH) -mabi=$(MABI) $(EFFORT) -Wall -ffunction-sections -fdata-sections -nostartfiles -mno-fdiv
CC_OPTS += -Wl,--gc-sections -lm -lc -lgcc -lc -g3
CC_OPTS += $(USER_FLAGS)


# -----------------------------------------------------------------------------
# Application output definitions
# -----------------------------------------------------------------------------
.PHONY: check info help elf_info clean clean_all bootloader
.DEFAULT_GOAL := help

# 'compile' is still here for compatibility
asm:     $(APP_ASM)
elf:     $(APP_ELF)
exe:     $(APP_EXE)
hex:     $(APP_HEX)
bin:     $(APP_BIN)
compile: $(APP_EXE)
image:   $(APP_IMG)
install: image install-$(APP_IMG)
all:     $(APP_ASM) $(APP_EXE) $(APP_IMG) install hex bin

# Check if making bootloader
# Use different base address and length for instruction memory/"rom" (BOOTROM instead of IMEM)
# Also define "make_bootloader" symbol for crt0.S, add debug symbols and use link-time optimization
target bootloader: CC_OPTS += -Wl,--defsym=make_bootloader=1 -Dmake_bootloader -g -flto
target bl_image:   CC_OPTS += -Wl,--defsym=make_bootloader=1 -Dmake_bootloader -g -flto


# -----------------------------------------------------------------------------
# Image generator targets
# -----------------------------------------------------------------------------
# install/compile tools
$(IMAGE_GEN): $(PICCOLO_SW_EXG_PATH)/image_gen.c
	@echo Compiling $(IMAGE_GEN)
	@$(CC_X86) $< -o $(IMAGE_GEN)


# -----------------------------------------------------------------------------
# General targets: Assemble, compile, link, dump
# -----------------------------------------------------------------------------
# Compile app *.s sources (assembly)
%.s.o: %.s
	@$(CC) -c $(CC_OPTS) -I $(PICCOLO_SW_INC_PATH) $(ASM_INC) $< -o $@

# Compile app *.S sources (assembly + C pre-processor)
%.S.o: %.S
	@$(CC) -c $(CC_OPTS) -I $(PICCOLO_SW_INC_PATH) $(ASM_INC) $< -o $@

# Compile app *.c sources
%.c.o: %.c
	@$(CC) -c $(CC_OPTS) -I $(PICCOLO_SW_INC_PATH) $(APP_INC) $< -o $@

# Compile app *.cpp sources
%.cpp.o: %.cpp
	@$(CC) -c $(CC_OPTS) -I $(PICCOLO_SW_INC_PATH) $(APP_INC) $< -o $@

# Link object files and show memory utilization
$(APP_ELF): $(OBJ)
	@$(CC) $(CC_OPTS) -T $(LD_SCRIPT) $(OBJ) -o $@ -lm
	@echo "Memory utilization:"
	@$(SIZE) $(APP_ELF)

# Assembly listing file (for debugging)
$(APP_ASM): $(APP_ELF)
	@$(OBJDUMP) -d -S -z  $< > $@

# Generate final executable from .text + .rodata + .data (in THIS order!)
main.bin: $(APP_ELF)
	@$(OBJCOPY) -I elf32-little $< -j .text   -O binary text.bin
	@$(OBJCOPY) -I elf32-little $< -j .rodata -O binary rodata.bin
	@$(OBJCOPY) -I elf32-little $< -j .data   -O binary data.bin
	@cat text.bin rodata.bin data.bin > $@
	@rm -f text.bin rodata.bin data.bin


# -----------------------------------------------------------------------------
# Application targets: Generate executable formats
# -----------------------------------------------------------------------------
# Generate Piccolo executable image for upload via bootloader
$(APP_EXE): main.bin $(IMAGE_GEN)
	@set -e
	@$(IMAGE_GEN) -app_bin $< $@ $(shell basename $(CURDIR))
	@echo "Executable ($(APP_EXE)) size in bytes:"
	@wc -c < $(APP_EXE)

# Generate Piccolo executable VHDL boot image
$(APP_IMG): main.bin $(IMAGE_GEN)
	@set -e
	@$(IMAGE_GEN) -app_img $< $@ $(shell basename $(CURDIR))

# Generate Piccolo RAW executable image in plain hex format
$(APP_HEX): main.bin $(IMAGE_GEN)
	@set -e
	@$(IMAGE_GEN) -raw_hex $< $@ $(shell basename $(CURDIR))

# Generate Piccolo RAW executable image in binary format
$(APP_BIN): main.bin $(IMAGE_GEN)
	@set -e
	@$(IMAGE_GEN) -raw_bin $< $@ $(shell basename $(CURDIR))

# -----------------------------------------------------------------------------
# Check toolchain
# -----------------------------------------------------------------------------
check: $(IMAGE_GEN)
	@echo "---------------- Check: Shell ----------------"
	@echo ${SHELL}
	@readlink -f ${SHELL}
	@echo "---------------- Check: $(CC) ----------------"
	@$(CC) -v
	@echo "---------------- Check: $(OBJDUMP) ----------------"
	@$(OBJDUMP) -V
	@echo "---------------- Check: $(OBJCOPY) ----------------"
	@$(OBJCOPY) -V
	@echo "---------------- Check: $(SIZE) ----------------"
	@$(SIZE) -V
	@echo "---------------- Check: Piccolo image_gen ----------------"
	@$(IMAGE_GEN) -help
	@echo "---------------- Check: Native GCC ----------------"
	@$(CC_X86) -v
	@echo
	@echo "Toolchain check OK"

# -----------------------------------------------------------------------------
# Show final ELF details (just for debugging)
# -----------------------------------------------------------------------------
elf_info: $(APP_ELF)
	@$(OBJDUMP) -x $(APP_ELF)

# -----------------------------------------------------------------------------
# Dump final ELF details (just for debugging)
# -----------------------------------------------------------------------------
elf_dump: $(APP_ELF)
	@$(OBJDUMP) -hdrt $(APP_ELF) > $(APP_ELF).dump

# -----------------------------------------------------------------------------
# Clean up
# -----------------------------------------------------------------------------
clean:
	@rm -f *.elf *.o *.bin *.out *.asm *.vhd *.hex

clean_all: clean
	@rm -f $(OBJ) $(IMAGE_GEN)


# -----------------------------------------------------------------------------
# Show configuration
# -----------------------------------------------------------------------------
info:
	@echo "---------------- Info: Project ----------------"
	@echo "Project folder:        $(shell basename $(CURDIR))"
	@echo "Source files:          $(APP_SRC)"
	@echo "Include folder(s):     $(APP_INC)"
	@echo "ASM include folder(s): $(ASM_INC)"
	@echo "---------------- Info: Piccolo ----------------"
	@echo "IMAGE_GEN: $(IMAGE_GEN)"
	@echo "Core source files:"
	@echo "$(CORE_SRC)"
	@echo "Core include folder:"
	@echo "$(PICCOLO_SW_INC_PATH)"
	@echo "---------------- Info: Objects ----------------"
	@echo "Project object files:"
	@echo "$(OBJ)"
	@echo "---------------- Info: RISC-V CPU ----------------"
	@echo "MARCH:      $(MARCH)"
	@echo "MABI:       $(MABI)"
	@echo "---------------- Info: Toolchain ----------------"
	@echo "Toolchain:  $(RISCV_TOLLCHAIN)"
	@echo "CC:         $(CC)"
	@echo "OBJDUMP:    $(OBJDUMP)"
	@echo "OBJCOPY:    $(OBJCOPY)"
	@echo "SIZE:       $(SIZE)"
	@echo "---------------- Info: Compiler Configuration ----------------"
	@$(CC) -v
	@echo "---------------- Info: Compiler Libraries ----------------"
	@echo "LIBGCC:"
	@$(CC) -print-libgcc-file-name
	@echo "SEARCH-DIRS:"
	@$(CC) -print-search-dirs
	@echo "---------------- Info: Flags ----------------"
	@echo "USER_FLAGS: $(USER_FLAGS)"
	@echo "CC_OPTS:    $(CC_OPTS)"
	@echo "---------------- Info: Host Native GCC Flags ----------------"
	@echo "CC_X86:     $(CC_X86)"


# -----------------------------------------------------------------------------
# Help
# -----------------------------------------------------------------------------
help:
	@echo "<<< Piccolo SW Application Makefile >>>"
	@echo "Make sure to add the bin folder of RISC-V GCC to your PATH variable."
	@echo ""
	@echo "=== Targets ==="
	@echo " help       - show this text"
	@echo " check      - check toolchain"
	@echo " info       - show makefile/toolchain configuration"
	@echo " asm        - compile and generate <main.asm> assembly listing file for manual debugging"
	@echo " elf        - compile and generate <main.elf> ELF file"
	@echo " exe        - compile and generate <piccolo_exe.bin> executable for upload via default bootloader (binary file, with header)"
	@echo " bin        - compile and generate <piccolo_raw_exe.bin> RAW executable file (binary file, no header)"
	@echo " hex        - compile and generate <piccolo_raw_exe.hex> RAW executable file (hex char file, no header)"
	@echo " image      - compile and generate VHDL IMEM boot image (for application, no header) in local folder"
	@echo " install    - compile, generate and install VHDL IMEM boot image (for application, no header)"
	@echo " sim        - in-console simulation using default/simple testbench and GHDL"
	@echo " all        - exe + install + hex + bin + asm"
	@echo " elf_info   - show ELF layout info"
	@echo " elf_dump   - dump ELF contents"
	@echo " clean      - clean up project home folder"
	@echo " clean_all  - clean up whole project, core libraries and image generator"
	@echo " bl_image   - compile and generate VHDL BOOTROM boot image (for bootloader only, no header) in local folder"
	@echo " bootloader - compile, generate and install VHDL BOOTROM boot image (for bootloader only, no header)"
	@echo ""
	@echo "=== Variables ==="
	@echo " USER_FLAGS   - Custom toolchain flags [append only], default \"$(USER_FLAGS)\""
	@echo " EFFORT       - Optimization level, default \"$(EFFORT)\""
	@echo " MARCH        - Machine architecture, default \"$(MARCH)\""
	@echo " MABI         - Machine binary interface, default \"$(MABI)\""
	@echo " APP_INC      - C include folder(s) [append only], default \"$(APP_INC)\""
	@echo " ASM_INC      - ASM include folder(s) [append only], default \"$(ASM_INC)\""
	@echo " RISCV_PREFIX - Toolchain prefix, default \"$(RISCV_PREFIX)\""
	@echo " PICCOLO_SW_HOME - PICCOLO SW home folder, default \"$(PICCOLO_SW_HOME)\""
	@echo ""
