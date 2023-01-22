#include <stdint.h>
#include <stdarg.h>

#define CLKRATE (100000000)
#define GPIO_BASE ((volatile uint32_t*)0xC0001000)

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

/* CLINT parameters */
#define MTIMECMP_BASE ((volatile uint32_t*)0x02004000)
#define MTIME_BASE ((volatile uint32_t*)0x0200BFF8)
#define MSIP_BASE ((volatile uint32_t*)0x02000000)

typedef struct __attribute__((packed,aligned(4))) {
  uint32_t CTRL;
  uint32_t DATA;
} neorv32_uart0_t;
#define UART_BASE (0xC0000000U)
#define UART (*((volatile neorv32_uart0_t*) (UART_BASE)))

/** UART0/UART1 control register bits */
enum NEORV32_UART_CTRL_enum {
  UART_CTRL_BAUD00   =  0, /**< UART control register(0)  (r/w): BAUD rate config value lsb (12-bit, bit 0) */
  UART_CTRL_BAUD01   =  1, /**< UART control register(1)  (r/w): BAUD rate config value (12-bit, bit 1) */
  UART_CTRL_BAUD02   =  2, /**< UART control register(2)  (r/w): BAUD rate config value (12-bit, bit 2) */
  UART_CTRL_BAUD03   =  3, /**< UART control register(3)  (r/w): BAUD rate config value (12-bit, bit 3) */
  UART_CTRL_BAUD04   =  4, /**< UART control register(4)  (r/w): BAUD rate config value (12-bit, bit 4) */
  UART_CTRL_BAUD05   =  5, /**< UART control register(5)  (r/w): BAUD rate config value (12-bit, bit 4) */
  UART_CTRL_BAUD06   =  6, /**< UART control register(6)  (r/w): BAUD rate config value (12-bit, bit 5) */
  UART_CTRL_BAUD07   =  7, /**< UART control register(7)  (r/w): BAUD rate config value (12-bit, bit 6) */
  UART_CTRL_BAUD08   =  8, /**< UART control register(8)  (r/w): BAUD rate config value (12-bit, bit 7) */
  UART_CTRL_BAUD09   =  9, /**< UART control register(9)  (r/w): BAUD rate config value (12-bit, bit 8) */
  UART_CTRL_BAUD10   = 10, /**< UART control register(10) (r/w): BAUD rate config value (12-bit, bit 9) */
  UART_CTRL_BAUD11   = 11, /**< UART control register(11) (r/w): BAUD rate config value msb (12-bit, bit 0) */
  UART_CTRL_SIM_MODE = 12, /**< UART control register(12) (r/w): Simulation output override enable, for use in simulation only */
  UART_CTRL_RX_EMPTY = 13, /**< UART control register(13) (r/-): RX FIFO is empty */
  UART_CTRL_RX_HALF  = 14, /**< UART control register(14) (r/-): RX FIFO is at least half-full */
  UART_CTRL_RX_FULL  = 15, /**< UART control register(15) (r/-): RX FIFO is full */
  UART_CTRL_TX_EMPTY = 16, /**< UART control register(16) (r/-): TX FIFO is empty */
  UART_CTRL_TX_HALF  = 17, /**< UART control register(17) (r/-): TX FIFO is at least half-full */
  UART_CTRL_TX_FULL  = 18, /**< UART control register(18) (r/-): TX FIFO is full */
  
  UART_CTRL_RTS_EN   = 20, /**< UART control register(20) (r/w): Enable hardware flow control: Assert RTS output if UART.RX is ready to receive */
  UART_CTRL_CTS_EN   = 21, /**< UART control register(21) (r/w): Enable hardware flow control: UART.TX starts sending only if CTS input is asserted */
  UART_CTRL_PMODE0   = 22, /**< UART control register(22) (r/w): Parity configuration (0=even; 1=odd) */
  UART_CTRL_PMODE1   = 23, /**< UART control register(23) (r/w): Parity bit enabled when set */
  UART_CTRL_PRSC0    = 24, /**< UART control register(24) (r/w): BAUD rate clock prescaler select bit 0 */
  UART_CTRL_PRSC1    = 25, /**< UART control register(25) (r/w): BAUD rate clock prescaler select bit 1 */
  UART_CTRL_PRSC2    = 26, /**< UART control register(26) (r/w): BAUD rate clock prescaler select bit 2 */
  UART_CTRL_CTS      = 27, /**< UART control register(27) (r/-): current state of CTS input */
  UART_CTRL_EN       = 28, /**< UART control register(28) (r/w): UART global enable */
  UART_CTRL_RX_IRQ   = 29, /**< UART control register(29) (r/w): RX IRQ mode: 1=FIFO at least half-full; 0=FIFO not empty */
  UART_CTRL_TX_IRQ   = 30, /**< UART control register(30) (r/w): TX IRQ mode: 1=FIFO less than half-full; 0=FIFO not full */
  UART_CTRL_TX_BUSY  = 31  /**< UART control register(31) (r/-): Transmitter is busy when set */
};

/** UART0/UART1 parity configuration */
enum NEORV32_UART_PARITY_enum {
  PARITY_NONE = 0b00, /**< 0b00: No parity bit at all */
  PARITY_EVEN = 0b10, /**< 0b10: Even parity */
  PARITY_ODD  = 0b11  /**< 0b11: Odd parity */
};

/** UART0/UART1 hardware flow control configuration */
enum NEORV32_UART_FLOW_CONTROL_enum {
  FLOW_CONTROL_NONE   = 0b00, /**< 0b00: No hardware flow control */
  FLOW_CONTROL_RTS    = 0b01, /**< 0b01: Assert RTS output if UART.RX is ready to receive */
  FLOW_CONTROL_CTS    = 0b10, /**< 0b10: UART.TX starts sending only if CTS input is asserted */
  FLOW_CONTROL_RTSCTS = 0b11  /**< 0b11: Assert RTS output if UART.RX is ready to receive & UART.TX starts sending only if CTS input is asserted */
};

/** UART0/UART1 receive/transmit data register bits */
enum NEORV32_UART_DATA_enum {
  UART_DATA_LSB   =  0, /**< UART receive/transmit data register(0)  (r/w): Receive/transmit data LSB (bit 0) */
  UART_DATA_MSB   =  7, /**< UART receive/transmit data register(7)  (r/w): Receive/transmit data MSB (bit 7) */

  UART_DATA_PERR  = 28, /**< UART receive/transmit data register(18) (r/-): RX parity error detected when set */
  UART_DATA_FERR  = 29, /**< UART receive/transmit data register(29) (r/-): RX frame error (no valid stop bit) detected when set */
  UART_DATA_OVERR = 30, /**< UART receive/transmit data register(30) (r/-): RX data overrun when set */
  UART_DATA_AVAIL = 31  /**< UART receive/transmit data register(31) (r/-): RX data available when set */
};

volatile uint64_t tohost;
char* msg = "Hello World\n";

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