/* ################################################################################################# */
/* # << Piccolo PLIC tests >>                                                                      # */
/* # ********************************************************************************************* # */
/* # BSD 3-Clause License                                                                          # */
/* #                                                                                               # */
/* # Copyright (c) 2022, Ashwin Menon. All rights reserved.                                        # */
/* #                                                                                               # */
/* # Redistribution and use in source and binary forms, with or without modification, are          # */
/* # permitted provided that the following conditions are met:                                     # */
/* #                                                                                               # */
/* # 1. Redistributions of source code must retain the above copyright notice, this list of        # */
/* #    conditions and the following disclaimer.                                                   # */
/* #                                                                                               # */
/* # 2. Redistributions in binary form must reproduce the above copyright notice, this list of     # */
/* #    conditions and the following disclaimer in the documentation and/or other materials        # */
/* #    provided with the distribution.                                                            # */
/* #                                                                                               # */
/* # 3. Neither the name of the copyright holder nor the names of its contributors may be used to  # */
/* #    endorse or promote products derived from this software without specific prior written      # */
/* #    permission.                                                                                # */
/* #                                                                                               # */
/* # THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS   # */
/* # OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF               # */
/* # MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE    # */
/* # COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,     # */
/* # EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE # */
/* # GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED    # */
/* # AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     # */
/* # NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED  # */
/* # OF THE POSSIBILITY OF SUCH DAMAGE.                                                            # */
/* # ********************************************************************************************* # */
/* # The Piccolo Processor - https://github.com/bluespec/Piccolo                      (c) Bluespec # */
/* ################################################################################################# */

#include <stdint.h>

#include "piccolo.h"

volatile uint64_t tohost;

void fail() {
  tohost = 2;
  while(1) { }
}

void plic_test() {
  uint32_t d;
  (void)d;

  // Write priorities
  for(int i=1; i<PLIC_NUM_SOURCES; i++) {
    *(PLIC_BASE + PLIC_PRIO_OFFS + i) = i;
  }
  // Read priorities
  for(int i=1; i<PLIC_NUM_SOURCES; i++) {
    d = *(PLIC_BASE + PLIC_PRIO_OFFS + i);
    if (d != (i&PLIC_MAX_PRIO)) fail();
  }

  // Read Interrupt pending status
  for(int i=0; i<PLIC_NUM_SOURCES; i+=32) {
    d = *(PLIC_BASE + PLIC_IPEND_OFFS + i);
    if (d) fail();
  }

  // Write Interrupt enable per source per target
  for(int i=0; i<PLIC_NUM_TARGETS; i++) {
    for(int j=0; j<PLIC_NUM_SOURCES; j+=32) {
      *(PLIC_BASE + PLIC_IENA_T0_OFFS + i*PLIC_IENABLK_SIZE + j) = 
          i*PLIC_IENABLK_SIZE + j;
    }
  }
  // Read Interrupt enable per source per target
  for(int i=0; i<PLIC_NUM_TARGETS; i++) {
    for(int j=0; j<PLIC_NUM_SOURCES; j+=32) {
      d = *(PLIC_BASE + PLIC_IENA_T0_OFFS + i*PLIC_IENABLK_SIZE + j);
      if (d != i*PLIC_IENABLK_SIZE + j) fail();
    }
  }

  // Write threshholds
  for(int i=0; i<PLIC_NUM_TARGETS; i++) {
    *(PLIC_BASE + PLIC_THRE_T0_OFFS + i*PLIC_THREBLK_SIZE) = i+1;
  }
  // Read threshholds
  for(int i=0; i<PLIC_NUM_TARGETS; i++) {
    d = *(PLIC_BASE + PLIC_THRE_T0_OFFS + i*PLIC_THREBLK_SIZE);
    if (d != i+1) fail();
  }

  // Read claim/complete
  for(int i=0; i<PLIC_NUM_TARGETS; i++) {
    d = *(PLIC_BASE + PLIC_CLCM_T0_OFFS + i*PLIC_CLCMBLK_SIZE);
    if (d) fail();
  }
}

int main() {
  // exercise PLIC
  plic_test();

  tohost = 1;
  return 0;
}