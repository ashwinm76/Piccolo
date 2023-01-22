#include <stdint.h>

#include "piccolo.h"

volatile uint64_t tohost;
char* msg = "Hello World\n";

void clint_test() {
  uint32_t d;
  (void)d;

  // Read MTIME
  for(int i=0; i<5; i++) {
    d = *(MTIME_BASE);
    uart_printf("MTIME=%d\n", d);
  }
  
  // Write MTIME
  *(MTIME_BASE) = 0;

  // Read MTIME
  for(int i=0; i<5; i++) {
    d = *(MTIME_BASE);
    uart_printf("MTIME=%d\n", d);
  }

  // Read MTIMECMP
  d = *(MTIMECMP_BASE);
  uart_printf("MTIMECMP=0x%x\n", d);
  // Write MTIMECMP
  *(MTIMECMP_BASE) = 0xdeadbeef;
  // Read MTIMECMP
  d = *(MTIMECMP_BASE);
  uart_printf("MTIMECMP=0x%x\n", d);

  // Read MSIP
  d = *(MSIP_BASE);
  uart_printf("MSIP=%d\n", d);
  // Write MSIP
  *(MSIP_BASE) = 1;
  // Read MSIP
  d = *(MSIP_BASE);
  uart_printf("MSIP=%d\n", d);
}

void plic_test() {
  uint32_t d;
  (void)d;

  // Write priorities
  uart_puts("PLIC Priorities WR\n");
  for(int i=1; i<PLIC_NUM_SOURCES; i++) {
    *(PLIC_BASE + PLIC_PRIO_OFFS + i) = i;
  }
  // Read priorities
  uart_puts("PLIC Priorities RD\n");
  for(int i=1; i<PLIC_NUM_SOURCES; i++) {
    d = *(PLIC_BASE + PLIC_PRIO_OFFS + i);
    uart_printf("Pri[%d] = %u\n", i, d);
  }

  // Read Interrupt pending status
  uart_puts("PLIC IPs RD\n");
  for(int i=0; i<PLIC_NUM_SOURCES; i+=32) {
    d = *(PLIC_BASE + PLIC_IPEND_OFFS + i);
    uart_printf("IP[%d] = %u\n", i, d);
  }

  // Write Interrupt enable per source per target
  uart_puts("PLIC IEs WR\n");
  for(int i=0; i<PLIC_NUM_TARGETS; i++) {
    for(int j=0; j<PLIC_NUM_SOURCES; j+=32) {
      *(PLIC_BASE + PLIC_IENA_T0_OFFS + i*PLIC_IENABLK_SIZE + j) = 
          i*PLIC_IENABLK_SIZE + j;
    }
  }
  // Read Interrupt enable per source per target
  uart_puts("PLIC IEs RD\n");
  for(int i=0; i<PLIC_NUM_TARGETS; i++) {
    for(int j=0; j<PLIC_NUM_SOURCES; j+=32) {
      d = *(PLIC_BASE + PLIC_IENA_T0_OFFS + i*PLIC_IENABLK_SIZE + j);
      uart_printf("IE[%d][%d] = %u\n", i, j, d);
    }
  }

  // Write threshholds
  uart_puts("PLIC Threshholds WR\n");
  for(int i=0; i<PLIC_NUM_TARGETS; i++) {
    *(PLIC_BASE + PLIC_THRE_T0_OFFS + i*PLIC_THREBLK_SIZE) = i+1;
  }
  // Read threshholds
  uart_puts("PLIC Threshholds RD\n");
  for(int i=0; i<PLIC_NUM_TARGETS; i++) {
    d = *(PLIC_BASE + PLIC_THRE_T0_OFFS + i*PLIC_THREBLK_SIZE);
    uart_printf("Thres[%d] = %u\n", i, d);
  }

  // Read claim/complete
  uart_puts("PLIC Claim/completes RD\n");
  for(int i=0; i<PLIC_NUM_TARGETS; i++) {
    d = *(PLIC_BASE + PLIC_CLCM_T0_OFFS + i*PLIC_CLCMBLK_SIZE);
    uart_printf("CLCM[%d] = %u\n", i, d);
  }
}

int main() {
  char* c = msg;

  uart_init(57600, PARITY_NONE, FLOW_CONTROL_NONE);

  // exercise the CLINT
  clint_test();

  // exercise PLIC
  plic_test();

  // print message
  while(*c) {
    uart_putc(*c++);
  }

  // wait for TX end
  while(uart_tx_busy()) { }

  // Get chars
  char k = 0;
  while(k != '\n') {
    k = uart_getc();
    //uart_putc(k);
  }

  // wait for TX end
  while(uart_tx_busy()) { }

  // exercise GPIO
  uint32_t i = 1;
  while(i) {
    *(volatile uint32_t*)(GPIO_BASE+2) = i;
    i <<= 1;
  }

  tohost = 1;
  return 0;
}