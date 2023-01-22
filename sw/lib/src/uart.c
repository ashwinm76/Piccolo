/* ################################################################################################# */
/* # << UART functionality for a Neorv32-like UART >>                                              # */
/* # Modified (by Ashwin Menon) from the NEORV32 UART header for the Piccolo processor             # */
/* #   - The NEORV32 Processor - https://github.com/stnolting/neorv32          (c) Stephan Nolting # */
/* # ********************************************************************************************* # */
/* # BSD 3-Clause License                                                                          # */
/* #                                                                                               # */
/* # Copyright (c) 2022, Stephan Nolting. All rights reserved.                                     # */
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

#include <stdarg.h>

#include "common.h"
#include "uart.h"

static void __uart_itoa(uint32_t x, char *res);
static void __uart_tohex(uint32_t x, char *res);
static void __uart_touppercase(uint32_t len, char *ptr);

void uart_init(uint32_t baudrate, uint8_t parity, uint8_t flow_con) {
  UART.CTRL = 0;

  uint32_t clock = CLKRATE;
  uint16_t i = 0; // BAUD rate divisor
  uint8_t p = 0; // initial prsc = CLK/2

  i = (uint16_t)(clock / (2*baudrate));
  
  // find baud prescaler (12-bit wide))
  while (i >= 0x0fff) {
    if ((p == 2) || (p == 4))
      i >>= 3;
    else
      i >>= 1;
    p++;
  }

  uint32_t clk_prsc = (uint32_t)p;
  clk_prsc = clk_prsc << UART_CTRL_PRSC0;

  uint32_t baud_prsc = (uint32_t)i;
  baud_prsc = baud_prsc - 1;
  baud_prsc = baud_prsc << UART_CTRL_BAUD00;

  uint32_t uart_en = 1;
  uart_en = uart_en << UART_CTRL_EN;

  uint32_t parity_config = (uint32_t)(parity & 3);
  parity_config = parity_config << UART_CTRL_PMODE0;

  uint32_t flow_control = (uint32_t)(flow_con & 3);
  flow_control = flow_control << UART_CTRL_RTS_EN;

  UART.CTRL = clk_prsc | baud_prsc | uart_en | parity_config | flow_control;
}

void uart_putc(char c) {
  // wait for previous transfer to finish
  while ((UART.CTRL & (1<<UART_CTRL_TX_FULL)) != 0); // wait for space in TX FIFO
  UART.DATA = ((uint32_t)c) << UART_DATA_LSB;
}

void uart_puts(char *s) {
  while(*s) {
    uart_putc(*s++);
  }
}

int uart_tx_busy(void) {

  uint32_t ctrl = UART.CTRL;

  if (((ctrl & (1<<UART_CTRL_TX_BUSY)) != 0) ||  // TX engine busy
      ((ctrl & (1<<UART_CTRL_TX_EMPTY)) == 0)) { // TX buffer not empty
    return 1;
  }
  return 0;
}

char uart_getc(void) {

  uint32_t d = 0;
  while (1) {
    d = UART.DATA;
    if ((d & (1<<UART_DATA_AVAIL)) != 0) { // char received?
      return (char)d;
    }
  }
}

void uart_printf(const char *format, ...) {
  char c, string_buf[11];
  int32_t n;

  va_list a;
  va_start(a, format);

  while ((c = *format++)) {
    if (c == '%') {
      c = *format++;
      switch (c) {
        case 's': // string
          uart_puts(va_arg(a, char*));
          break;
        case 'c': // char
          uart_putc((char)va_arg(a, int));
          break;
        case 'i': // 32-bit signed
        case 'd':
          n = (int32_t)va_arg(a, int32_t);
          if (n < 0) {
            n = -n;
            uart_putc('-');
          }
          __uart_itoa((uint32_t)n, string_buf);
          uart_puts(string_buf);
          break;
        case 'u': // 32-bit unsigned
          __uart_itoa(va_arg(a, uint32_t), string_buf);
          uart_puts(string_buf);
          break;
        case 'x': // 32-bit hexadecimal
        case 'p':
        case 'X':
          __uart_tohex(va_arg(a, uint32_t), string_buf);
          if (c == 'X') {
            __uart_touppercase(11, string_buf);
          }
          uart_puts(string_buf);
          break;
        default: // unsupported format
          uart_putc('%');
          uart_putc(c);
          break;
      }
    }
    else {
      uart_putc(c);
    }
  }
  va_end(a);
}

static void __uart_itoa(uint32_t x, char *res) {
  static const char numbers[] = "0123456789";
  char buffer1[11];
  uint16_t i, j;

  buffer1[10] = '\0';
  res[10] = '\0';

  // convert
  for (i=0; i<10; i++) {
    buffer1[i] = numbers[x%10];
    x /= 10;
  }

  // delete 'leading' zeros
  for (i=9; i!=0; i--) {
    if (buffer1[i] == '0')
      buffer1[i] = '\0';
    else
      break;
  }

  // reverse
  j = 0;
  do {
    if (buffer1[i] != '\0')
      res[j++] = buffer1[i];
  } while (i--);

  res[j] = '\0'; // terminate result string
}

static void __uart_tohex(uint32_t x, char *res) {
  static const char symbols[] = "0123456789abcdef";
  int i;
  for (i=0; i<8; i++) { // nibble by nibble
    uint32_t num_tmp = x >> (4*i);
    res[7-i] = (char)symbols[num_tmp & 0x0f];
  }

  res[8] = '\0'; // terminate result string
}

static void __uart_touppercase(uint32_t len, char *ptr) {
  char tmp;
  while (len > 0) {
    tmp = *ptr;
    if ((tmp >= 'a') && (tmp <= 'z')) {
      *ptr = tmp - 32;
    }
    ptr++;
    len--;
  }
}
