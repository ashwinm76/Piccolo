#include <stdint.h>

#define CLKRATE (100000000)
#define GPIO_BASE ((volatile uint32_t*)0xC0001000)

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

uint64_t tohost;
char* msg = "Hello World\n";

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

int main() {
  char* c = msg;

  uart_init(57600, PARITY_EVEN, FLOW_CONTROL_RTSCTS);

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