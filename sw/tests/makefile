#################################################################################################
# << Piccolo - Tests Makefile >>                                                                #
# Modified (by Ashwin Menon) from the NEORV32 Application Makefile                              #
#   - The NEORV32 Processor - https://github.com/stnolting/neorv32          (c) Stephan Nolting #
# ********************************************************************************************* #
# BSD 3-Clause License                                                                          #
#                                                                                               #
# Copyright (c) 2021, Stephan Nolting. All rights reserved.                                     #
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

# Modify this variable to fit your Piccolo setup (Piccolo home folder)
PICCOLO_SW_HOME ?= ../

include $(PICCOLO_SW_HOME)/common/common.mk

TESTS_SRC ?= $(wildcard ./*.c)

TESTS_TARGETS = $(addsuffix .elf, $(notdir $(basename $(TESTS_SRC))))
TEST_DEST_DIR = ../../Tests/func
TEST_ARCH_PREFIX = rv32ui-p-

tests: $(TESTS_TARGETS)

%.elf: %.c
	@make APP_SRC=$< elf elf_dump
	@mv main.elf $(TEST_DEST_DIR)/$(TEST_ARCH_PREFIX)$(basename $@)
	@mv main.elf.dump $(TEST_DEST_DIR)/$(addsuffix .dump, $(TEST_ARCH_PREFIX)$(basename $@))
