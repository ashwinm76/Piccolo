/* ################################################################################################# */
/* # << PLIC functionality for a Piccolo PLIC >>                                                   # */
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

#ifndef PICCOLO_SW_LIB_INCLUDE_PLIC_H_
#define PICCOLO_SW_LIB_INCLUDE_PLIC_H_

/* PLIC parameters */
#define PLIC_NUM_SOURCES (16)
#define PLIC_NUM_TARGETS (2)
#define PLIC_MAX_PRIO (7)
#define PLIC_BASE ((volatile uint32_t*)0x0C000000)

/* PLIC register word offsets */
#define PLIC_PRIO_OFFS (0/4)
#define PLIC_IPEND_OFFS (0x1000/4)
#define PLIC_IENA_T0_OFFS (0x2000/4)
#define PLIC_IENABLK_SIZE (0x80/4)
#define PLIC_THRE_T0_OFFS (0x00200000/4)
#define PLIC_THREBLK_SIZE (0x1000/4)
#define PLIC_CLCM_T0_OFFS (0x00200004/4)
#define PLIC_CLCMBLK_SIZE (0x1000/4)

#endif
