// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved
// Copyright (c) 2022 Ashwin Menon. All Rights Reserved
// Created using UART_Model.bsv as a template

package UART_Neorv32_Model;

// ================================================================
// This package implements a slave IP, a UART model.
//
// This UART models the one available in the NEORV32 processor.
//
// Bus interface width: This slave IP can be attached to fabrics with
// 32b- or 64b-wide data channels.  The type parameter 'Wd_Data' in
// Fabric_Defs.bsv specifies this.
//
// Some of the 'truncate()'s and 'zeroExtend()'s below are no-ops but
// necessary to satisfy type-checking, to manage these width
// variations.
// ================================================================

export UARTNeorv32_IFC (..), mkUARTNeorv32;

// ================================================================
// BSV library imports

import  Vector        :: *;
import  FIFOF         :: *;
import  GetPut        :: *;
import  ClientServer  :: *;
import  ConfigReg     :: *;
import  StmtFSM       :: *;
import  FIFOLevel     :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import AXI4_Types  :: *;
import Fabric_Defs :: *;

// ================================================================
// UART types, registers and their address offsets

typedef enum {
  PRE_2,
  PRE_4,
  PRE_8,
  PRE_64,
  PRE_128,
  PRE_1024,
  PRE_2048,
  PRE_4096
} Prescaler deriving(Bits, Eq);

typedef enum {
  PAR_NONE,  /* No parity */
  PAR_NONE1, /* Also no parity */
  PAR_EVEN,  /* Even parity */
  PAR_ODD    /* Odd parity */
} ParityMode deriving(Bits, Eq);

typedef  enum {
  NOT_EMPTY, /* FIFO not empty */
  HALF_FULL  /* FIFO at least half full */
} RxIrqMode deriving(Bits, Eq);

typedef  enum {
  NOT_FULL, /* FIFO not full */
  HALF_FULL /* FIFO less than half full */
} TxIrqMode deriving(Bits, Eq);

typedef struct {
  TxIrqMode tx_irq_mode;
  RxIrqMode rx_irq_mode;
  Bool enable;
  Prescaler prescaler;
  ParityMode parity_mode;
  Bool cts_enable;
  Bool rts_enable;
  UInt#(12) baud;
} UartParams;

typedef struct {
  Bool tx_full;
  Bool tx_half_full;
  Bool tx_empty;
  Bool rx_full;
  Bool rx_half_full;
  Bool rx_empty;
} UartFifoStatus;

typedef struct {
  Bool tx_busy;
  Bool cts;
  UartFifoStatus fifo_status;
} UartStatus;

typedef struct {
  Bool rx_data_available;
  Bool rx_overrun_error;
  Bool rx_frame_error;
  Bool rx_parity_error;
} UartRxdStatus;

instance Bits#(UartParams, 32);
  function Bit#(32) pack(UartParams r);
    return {
      1'b0,
      pack(r.tx_irq_mode),
      pack(r.rx_irq_mode),
      pack(r.enable),
      1'b0,
      pack(r.prescaler),
      pack(r.parity_mode),
      pack(r.cts_enable),
      pack(r.rts_enable),
      1'b0,
      1'b0,
      1'b0,
      1'b0,
      1'b0,
      1'b0,
      1'b0,
      1'b0,
      pack(r.baud)
    };
  endfunction

  function UartParams unpack(Bit#(32) v);
    return UartParams {
      tx_irq_mode  : unpack(v[30]),
      rx_irq_mode  : unpack(v[29]),
      enable       : unpack(v[28]),
      prescaler    : unpack(v[26:24]),
      parity_mode  : unpack(v[23:22]),
      cts_enable   : unpack(v[21]),
      rts_enable   : unpack(v[20]),
      baud         : unpack(v[11:0])
    };
  endfunction
endinstance

instance Bits#(UartFifoStatus, 32);
  function Bit#(32) pack(UartFifoStatus r);
    return {
      1'b0,
      1'b0,
      1'b0,
      1'b0,
      1'b0,
      3'b0,
      2'b0,
      1'b0,
      1'b0,
      1'b0,
      pack(r.tx_full),
      pack(r.tx_half_full),
      pack(r.tx_empty),
      pack(r.rx_full),
      pack(r.rx_half_full),
      pack(r.rx_empty),
      1'b0,
      12'b0
    };
  endfunction

  function UartFifoStatus unpack(Bit#(32) v);
    return UartFifoStatus {
      tx_full      : unpack(v[18]),
      tx_half_full : unpack(v[17]),
      tx_empty     : unpack(v[16]),
      rx_full      : unpack(v[15]),
      rx_half_full : unpack(v[14]),
      rx_empty     : unpack(v[13])
    };
  endfunction
endinstance

instance Bits#(UartStatus, 32);
  function Bit#(32) pack(UartStatus r);
    return {
      pack(r.tx_busy),
      1'b0,
      1'b0,
      1'b0,
      pack(r.cts),
      3'b0,
      2'b0,
      1'b0,
      1'b0,
      1'b0,
      pack(r.fifo_status.tx_full),
      pack(r.fifo_status.tx_half_full),
      pack(r.fifo_status.tx_empty),
      pack(r.fifo_status.rx_full),
      pack(r.fifo_status.rx_half_full),
      pack(r.fifo_status.rx_empty),
      1'b0,
      12'b0
    };
  endfunction

  function UartStatus unpack(Bit#(32) v);
    return UartStatus {
      tx_busy      : unpack(v[31]),
      cts          : unpack(v[27]),
      fifo_status  : UartFifoStatus {
        tx_full      : unpack(v[18]),
        tx_half_full : unpack(v[17]),
        tx_empty     : unpack(v[16]),
        rx_full      : unpack(v[15]),
        rx_half_full : unpack(v[14]),
        rx_empty     : unpack(v[13])
      }
    };
  endfunction
endinstance

instance Bits#(UartRxdStatus, 32);
  function Bit#(32) pack(UartRxdStatus r);
    return {
       pack(r.rx_data_available),
       pack(r.rx_overrun_error),
       pack(r.rx_frame_error),
       pack(r.rx_parity_error),
       20'b0,
       8'b0
    };
  endfunction

  function UartRxdStatus unpack(Bit#(32) v);
    return UartRxdStatus {
      rx_data_available : unpack(v[31]),
      rx_overrun_error  : unpack(v[30]),
      rx_frame_error    : unpack(v[29]),
      rx_parity_error   : unpack(v[28])
    };
  endfunction
endinstance

typedef struct {
  Bool frame_error;
  Bool parity_error;
  Bit#(8) data;
} UartRxData deriving(Bits);

Bit#(3) addr_UART_ctrl  = 3'h_0;    // Control register
Bit#(3) addr_UART_data  = 3'h_1;    // Data register

// ================================================================
// THIS MODULE's INTERFACE

interface UARTNeorv32_IFC#(numeric type rxFifoSize, numeric type txFifoSize);
  // Reset
  interface Server #(Bit #(0), Bit #(0))  server_reset;

  // set_addr_map should be called after this module's reset
  method Action set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim);

  // Main Fabric Reqs/Rsps
  interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave;

  // TX signals
  (* always_ready *)
  method Bit#(1) txd_o; // out
  (* always_ready, always_enabled, prefix = "" *)
  method Action m_cts_i(Bit#(1) cts_i); // in

  // RX signals
  (* always_ready, always_enabled, prefix = "" *)
  method Action m_rxd_i(Bit#(1) rxd_i); // in
  (* always_ready *)
  method Bit#(1) rts_o; // out

  // Interrupt signal
  (* always_ready *)
  method Bool intr; // out
endinterface

// ================================================================
// Local types and constants

// Module state
typedef enum {
  STATE_START,
  STATE_READY
} Module_State deriving (Bits, Eq, FShow);

// ================================================================
// Addressing of UART registers

// ----------------------------------------------------------------
// UART reg addresses should be at stride 4 or 8.

Integer address_stride = 4;
// Integer address_stride = 8;

// ----------------------------------------------------------------
// Split a bus address into (offset, lsbs), based on the address
// stride.

function Tuple2 #(Bit#(64), Bit#(3)) split_addr (Bit#(64) addr);
  Bit#(64) offset = ((address_stride == 4) ? (addr >> 2)           : (addr >> 3));
  Bit#(3)  lsbs   = ((address_stride == 4) ? { 1'b0, addr [1:0] }  : addr [2:0]);

   return tuple2 (offset, lsbs);
endfunction

// ----------------------------------------------------------------
// Extract data from AXI4 byte lanes, based on the AXI4 'strobe'
// (byte-enable) bits.

function Bit #(64) fn_extract_AXI4_data (Bit #(64) data, Bit #(8) strb);
  Bit#(64) result = 0;
  case (strb)
    8'b_0000_0001: result = zeroExtend (data [ 7:0]);
    8'b_0000_0010: result = zeroExtend (data [15:8]);
    8'b_0000_0100: result = zeroExtend (data [23:16]);
    8'b_0000_1000: result = zeroExtend (data [31:24]);
    8'b_0001_0000: result = zeroExtend (data [39:32]);
    8'b_0010_0000: result = zeroExtend (data [47:40]);
    8'b_0100_0000: result = zeroExtend (data [55:48]);
    8'b_1000_0000: result = zeroExtend (data [63:56]);

    8'b_0000_0011: result = zeroExtend (data [15:0]);
    8'b_0000_1100: result = zeroExtend (data [31:16]);
    8'b_0011_0000: result = zeroExtend (data [47:32]);
    8'b_1100_0000: result = zeroExtend (data [63:48]);

    8'b_0000_1111: result = zeroExtend (data [31:0]);
    8'b_1111_0000: result = zeroExtend (data [63:32]);

    8'b_1111_1111: result = zeroExtend (data [63:0]);
  endcase
  return result;
endfunction

// ================================================================
// THIS MODULE's IMPLEMENTATION

module mkUARTNeorv32(UARTNeorv32_IFC#(rxFifoSize, txFifoSize));
  Reg#(Bit#(8)) cfg_verbosity <- mkConfigReg (0);
  Reg#(Module_State) rg_state <- mkReg (STATE_START);

  // These regs represent where this UART is placed in the address space.
  Reg#(Fabric_Addr) rg_addr_base <- mkRegU;
  Reg#(Fabric_Addr) rg_addr_lim  <- mkRegU;

  FIFOF#(Bit #(0)) f_reset_reqs <- mkFIFOF;
  FIFOF#(Bit #(0)) f_reset_rsps <- mkFIFOF;
  FIFOLevelIfc#(Bit#(8), txFifoSize) tx_fifo <- mkFIFOLevel;
  FIFOLevelIfc#(UartRxData, rxFifoSize) rx_fifo <- mkFIFOLevel;
  Reg#(UartFifoStatus) rg_fifo_status[2] <- mkCRegU(2);

  // ----------------
  // Connector to AXI4 fabric
  AXI4_Slave_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave_xactor <- mkAXI4_Slave_Xactor;

  // ----------------
  // UART registers
  Reg#(UartParams) rg_params <- mkRegU;  // Operation parameters
  
  Reg#(Bool) rg_tx_busy <- mkRegU;
  Reg#(Bool) rg_cts <- mkRegU;
  Reg#(Bool) rg_rx_overr[2] <- mkCReg(2, False);

  Reg#(Bit#(11)) rg_txd <- mkRegU;       // TX data register
  Reg#(Bit#(10)) rg_rxd <- mkRegU;       // RX data register

  // TX counters
  Reg#(UInt#(4)) rg_txbits <- mkRegU;    // TX bit counter
  Reg#(UInt#(12)) rg_txbaud <- mkRegU;   // TX baud counter

  // RX counters
  Reg#(UInt#(4)) rg_rxbits <- mkRegU;    // RX bit counter
  Reg#(UInt#(12)) rg_rxbaud <- mkRegU;   // RX baud counter
   
  // Input synchronizer registers
  Reg#(Bit#(1)) rg_cts_sync <- mkRegU;   // CTS
  Reg#(Bit#(5)) rg_rxd_sync <- mkRegU;   // RX data

  // RX rtr
  Reg#(Bool) rg_rtr <- mkReg(False);

  // Clock dividers
  Reg#(Bit#(12)) rg_clk_div <- mkReg(0);
  Reg#(Bit#(12)) rg_clk_div_ff <- mkReg(0);

  // ----------------
  // Test if an interrupt is pending
  function Bool fn_intr ();
    // TODO
    return False;
  endfunction

  // Get the UART status
  function UartStatus get_status;
    return UartStatus {
      tx_busy: rg_tx_busy,
      cts: rg_cts,
      fifo_status: rg_fifo_status[1]
    };
  endfunction

  // Get the UART RX data status
  function UartRxdStatus get_rxd_status;
    Bool frame_error = False;
    Bool parity_error = False;

    if (!rg_fifo_status[1].rx_empty) begin
       frame_error = rx_fifo.first.frame_error;
       parity_error = rx_fifo.first.parity_error;
    end
    let ret = UartRxdStatus {
       rx_data_available: !rg_fifo_status[1].rx_empty,
       rx_overrun_error: rg_rx_overr[0],
       rx_frame_error: frame_error,
       rx_parity_error: parity_error
    };
    return ret;
  endfunction

  // ================================================================
  // BEHAVIOR

  // ----------------------------------------------------------------
  // Soft reset (on token in f_reset_reqs)
  rule rl_reset;
    f_reset_reqs.deq;

    slave_xactor.reset;
    rg_state <= STATE_READY;

    f_reset_rsps.enq (?);

    if (cfg_verbosity != 0)
      $display ("%0d: UART.rl_reset", cur_cycle);
  endrule

  // ----------------------------------------------------------------
  // Handle fabric read requests

  rule rl_process_rd_req (rg_state == STATE_READY);
    let rda <- pop_o(slave_xactor.o_rd_addr);
    let byte_addr = rda.araddr - rg_addr_base;
    match { .offset, .lsbs } = split_addr(zeroExtend(byte_addr));

    Bit#(32)  rdata_word = 0;
    AXI4_Resp rresp = axi4_resp_okay;

    if ((rda.araddr < rg_addr_base) || (rda.araddr >= rg_addr_lim)) begin
      $display ("%0d: %m.rl_process_rd_req: ERROR: UART addr out of bounds", cur_cycle);
      $display ("    UART base addr 0x%0h  limit addr 0x%0h", rg_addr_base, rg_addr_lim);
      $display ("    AXI4 request: ", fshow(rda));
      rresp = axi4_resp_decerr;
    end
    else if (lsbs != 0) begin
      $display ("%0d: %m.rl_process_rd_req: ERROR: UART misaligned addr", cur_cycle);
      $display ("            ", fshow(rda));
      rresp = axi4_resp_slverr;
    end
    else if (offset [63:3] != 0) begin
      $display ("%0d: %m.rl_process_rd_req: ERROR: UART unsupported addr", cur_cycle);
      $display ("            ", fshow(rda));
      rresp = axi4_resp_decerr;
    end
    // offset 0: Ctrl
    else if (offset [2:0] == addr_UART_ctrl) begin
      rdata_word = pack(rg_params) | pack(get_status);
    end
    // offset 1: Data
    else if (offset [2:0] == addr_UART_data) begin
      rdata_word = pack(get_rxd_status);
      // Reset the RX overrun status on read
      rg_rx_overr[0] <= False;
      if (!rg_fifo_status[1].rx_empty) begin
        rdata_word[7:0] = rx_fifo.first.data[7:0];
        rx_fifo.deq;
      end
    end
    else begin
      $display ("%0d: %m.rl_process_rd_req: ERROR: UART unsupported addr", cur_cycle);
      $display ("            ", fshow (rda));
      rresp = axi4_resp_decerr;
    end

    // Align data byte for AXI4 data bus based on fabric-width
    Fabric_Data rdata = zeroExtend (rdata_word);
    if ((valueOf(Wd_Data) == 64) && (byte_addr[2:0] == 3'b100))
      rdata = rdata << 32;

    // Send read-response to bus
    let rdr = AXI4_Rd_Data{
      rid:   rda.arid,
      rdata: rdata,
      rresp: rresp,
      rlast: True,
      ruser: rda.aruser};
    slave_xactor.i_rd_data.enq (rdr);

    if (cfg_verbosity > 1) begin
      $display ("%0d: %m.rl_process_rd_req", cur_cycle);
      $display ("            ", fshow(rda));
      $display ("            ", fshow(rdr));
    end
  endrule

  // ----------------------------------------------------------------
  // Handle fabric write requests
  rule rl_process_wr_req (rg_state == STATE_READY);
    let wra <- pop_o (slave_xactor.o_wr_addr);
    let wrd <- pop_o (slave_xactor.o_wr_data);

    Bit#(64) wdata = zeroExtend(wrd.wdata);
    Bit#(8) wstrb = zeroExtend(wrd.wstrb);
    Bit#(32) data_word = truncate(fn_extract_AXI4_data(wdata, wstrb));

    let byte_addr = wra.awaddr - rg_addr_base;
    match { .offset, .lsbs } = split_addr(zeroExtend(byte_addr));

    AXI4_Resp bresp = axi4_resp_okay;

    if ((wra.awaddr < rg_addr_base) || (wra.awaddr >= rg_addr_lim)) begin
      $display ("%0d: %m.rl_process_rd_req: ERROR: UART addr out of bounds", cur_cycle);
      $display ("    UART base addr 0x%0h  limit addr 0x%0h", rg_addr_base, rg_addr_lim);
      $display ("    AXI4 request: ", fshow (wra));
      bresp = axi4_resp_decerr;
    end
    else if (lsbs != 0) begin
      $display ("%0d: %m.rl_process_wr_req: ERROR: UART misaligned addr", cur_cycle);
      $display ("            ", fshow(wra));
      bresp = axi4_resp_slverr;
    end
    else if (offset [63:3] != 0) begin
      $display ("%0d: %m.rl_process_wr_req: ERROR: UART unsupported addr", cur_cycle);
      $display ("            ", fshow(wra));
      bresp = axi4_resp_decerr;
    end
    // offset 0: Ctrl
    else if (offset [2:0] == addr_UART_ctrl) begin
      rg_params <= unpack(data_word);
    end
    // offset 1: Data
    else if (offset [2:0] == addr_UART_data) begin
      tx_fifo.enq(data_word[7:0]);
    end
    else begin
      $display ("%0d: %m.rl_process_wr_req: ERROR: UART unsupported addr", cur_cycle);
      $display ("            ", fshow(wra));
      $display ("            ", fshow(wrd));
      bresp = axi4_resp_decerr;
    end

    // Send write-response to bus
    let wrr = AXI4_Wr_Resp{
      bid:   wra.awid,
      bresp: bresp,
      buser: wra.awuser};
    slave_xactor.i_wr_resp.enq(wrr);

    if (cfg_verbosity > 1) begin
      $display ("%0d: %m.rl_process_wr_req", cur_cycle);
      $display ("            ", fshow(wra));
      $display ("            ", fshow(wrd));
      $display ("            ", fshow(wrr));
    end
  endrule

  // ----------------------------------------------------------------
  // Clock divider

  function Bool prescaled_tick ();
    case (rg_params.prescaler)
      PRE_2: return unpack(rg_clk_div[0] & ~rg_clk_div_ff[0]);
      PRE_4: return unpack(rg_clk_div[1] & ~rg_clk_div_ff[1]);
      PRE_8: return unpack(rg_clk_div[2] & ~rg_clk_div_ff[2]);
      PRE_64: return unpack(rg_clk_div[5] & ~rg_clk_div_ff[5]);
      PRE_128: return unpack(rg_clk_div[6] & ~rg_clk_div_ff[6]);
      PRE_1024: return unpack(rg_clk_div[9] & ~rg_clk_div_ff[9]);
      PRE_2048: return unpack(rg_clk_div[10] & ~rg_clk_div_ff[10]);
      PRE_4096: return unpack(rg_clk_div[11] & ~rg_clk_div_ff[11]);
    endcase
  endfunction

  rule rl_clk_divider;
    rg_clk_div <= rg_clk_div + 1;
    rg_clk_div_ff <= rg_clk_div;
  endrule

  function UInt#(4) getbits;
    return (rg_params.parity_mode == PAR_NONE || rg_params.parity_mode == PAR_NONE1) ? 10 : 11 ;
  endfunction

  // ----------------------------------------------------------------
  // TX FSM
  mkAutoFSM(seq
    while(True) seq
      action
        rg_txd <= 1;
        rg_tx_busy <= False;
      endaction

      await(rg_params.enable && tx_fifo.notEmpty && rg_cts);

      // Prepare for TX
      action
        Bit#(1) p = 1;
        if (rg_params.parity_mode == PAR_ODD || rg_params.parity_mode == PAR_EVEN) begin
          p = reduceXor(tx_fifo.first);
          if (rg_params.parity_mode == PAR_ODD) p = ~p;
        end
        rg_txd <= {1'b1, p, tx_fifo.first, 1'b0};
        tx_fifo.deq;
        rg_txbits <= getbits;
        rg_tx_busy <= True;
        rg_txbaud <= rg_params.baud;
      endaction

      if (!rg_params.enable) continue;
      
      // TX all bits
      while (rg_txbits > 0) seq
        if (!rg_params.enable) break;
        if (prescaled_tick()) seq
          if (rg_txbaud == 0) action
            rg_txbaud <= rg_params.baud;
            rg_txd <= rg_txd >> 1;
            rg_txbits <= rg_txbits - 1;
          endaction
          else 
            rg_txbaud <= rg_txbaud - 1;
        endseq
        else noAction;
      endseq
      
    endseq
  endseq);

  // ----------------------------------------------------------------
  // RX FSM
  mkAutoFSM(seq
    while(True) seq
      if (!rg_params.enable || rg_fifo_status[1].rx_full) action
        rg_rtr <= False;
      endaction
      else seq
        // Prepare for RX
        action
          rg_rxbaud <= rg_params.baud >> 1;
          rg_rxbits <= getbits;
          rg_rtr <= True;
        endaction

        if (!rg_params.enable) continue;
        
        // Wait for the start bit
        await(rg_rxd_sync[3:0] == 4'b0011);
        
        // Deassert RTR during the RX
        rg_rtr <= False;

        if (!rg_params.enable) continue;
        
        // RX all bits
        while(rg_rxbits > 0) seq
          if (!rg_params.enable) break;
          if (prescaled_tick()) seq
            if (rg_rxbaud == 0) action
              rg_rxbaud <= rg_params.baud;
              rg_rxbits <= rg_rxbits - 1;
              rg_rxd <= {rg_rxd_sync[2], rg_rxd[9:1]};
            endaction
            else
              rg_rxbaud <= rg_rxbaud - 1;
          endseq
          else noAction;
        endseq

        if (!rg_params.enable) continue;
      
        // Store RX data and check parity
        action
          Bit#(8) data;
          Bool perr = False;
          if ((rg_params.parity_mode == PAR_NONE) || (rg_params.parity_mode == PAR_NONE1))
            data = rg_rxd[8:1];
          else begin
            data = rg_rxd[7:0];
            perr = unpack(reduceXor(rg_rxd[8:0]));
            if (rg_params.parity_mode == PAR_ODD) perr = !perr;
          end
          if (!rg_fifo_status[1].rx_full) begin
            rx_fifo.enq(UartRxData {
              frame_error : unpack(~rg_rxd[9]),
              parity_error : perr,
              data : data
            });
          end
          else
            rg_rx_overr[1] <= True;
        endaction
      endseq
    endseq
  endseq);

  rule fifo_status;
    rg_fifo_status[0] <= UartFifoStatus {
      tx_full: !tx_fifo.notFull,
      tx_half_full: tx_fifo.isGreaterThan(valueOf(txFifoSize)/2-1),
      tx_empty: !tx_fifo.notEmpty,
      rx_full: !rx_fifo.notFull,
      rx_half_full: rx_fifo.isGreaterThan(valueOf(rxFifoSize)/2-1),
      rx_empty: !rx_fifo.notEmpty
    };
  endrule

  // ================================================================
  // INTERFACE

  // Reset
  interface server_reset = toGPServer(f_reset_reqs, f_reset_rsps);

  // set_addr_map should be called after this module's reset
  method Action set_addr_map(Fabric_Addr addr_base, Fabric_Addr addr_lim);
    if (addr_base [2:0] != 0)
      $display ("%0d: WARNING: UART.set_addr_map: addr_base 0x%0h is not 8-Byte-aligned",
		   cur_cycle, addr_base);

    if (addr_lim [2:0] != 0)
      $display ("%0d: WARNING: UART.set_addr_map: addr_lim 0x%0h is not 8-Byte-aligned",
		   cur_cycle, addr_lim);

    rg_addr_base <= addr_base;
    rg_addr_lim  <= addr_lim;
  endmethod

  // Main Fabric Reqs/Rsps
  interface slave = slave_xactor.axi_side;

  // TX signals
  method Bit#(1) txd_o; // out
    return rg_txd[0];
  endmethod

  method Action m_cts_i(Bit#(1) cts_i); // in
    rg_cts_sync <= cts_i;
    // CTS pin is active low
    // CTS internal signal is active high
    rg_cts <= rg_params.cts_enable ? !unpack(rg_cts_sync) : True;
  endmethod

  // RX signals
  method Action m_rxd_i(Bit#(1) rxd_i); // in
    rg_rxd_sync <= {rxd_i, rg_rxd_sync[4:1]};
  endmethod

  method Bit#(1) rts_o; // out
    // RTS pin is active low
    // RTS internal signal is active high
    return rg_params.rts_enable ? pack(!rg_rtr) : 0;
  endmethod

  // Interrupt pending
  method Bool intr;
    return fn_intr ();
  endmethod
endmodule

// ================================================================

endpackage
