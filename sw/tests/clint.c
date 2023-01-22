/* ################################################################################################# */
/* # << Piccolo CLINT tests >>                                                                     # */
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

#include "piccolo.h"

volatile uint64_t tohost;

void fail() {
  tohost = 2;
  while(1) { }
}

void clint_test() {
  uint32_t d, d1;
  (void)d;

  // Read MTIME
  d1 = 0;
  for(int i=0; i<5; i++) {
    d = *(MTIME_BASE);
    // timer must always increase
    if (d < d1) fail();
    d1 = d;
  }
  
  // Write MTIME
  *(MTIME_BASE) = 0;

  // Read MTIME
  d = *(MTIME_BASE);
  // resetting timer to 0 must make it smaller than the last read value
  if (d >= d1) fail();

  // Read MTIME
  d1 = 0;
  for(int i=0; i<5; i++) {
    d = *(MTIME_BASE);
    // timer must always increase
    if (d < d1) fail();
    d1 = d;
  }

  // Read MTIMECMP
  d = *(MTIMECMP_BASE);
  if (d) fail();

  // Write MTIMECMP
  *(MTIMECMP_BASE) = 0xdeadbeef;
  
  // Read MTIMECMP
  d = *(MTIMECMP_BASE);
  if (d != 0xdeadbeef) fail();

  // Read MSIP
  d = *(MSIP_BASE);
  if (d) fail();

  // Write MSIP
  *(MSIP_BASE) = 1;
  
  // Read MSIP
  d = *(MSIP_BASE);
  if (!d) fail();
}

int main() {
  // exercise CLINT
  clint_test();

  tohost = 1;
  return 0;
}