// Copyright (c) 2022 Ashwin Menon. All Rights Reserved

package Uart_TB_Model;

import StmtFSM::*;
import FIFOF::*;

  typedef enum {
    PRE_2,
    PRE_4,
    PRE_8,
    PRE_64,
    PRE_128,
    PRE_1024,
    PRE_2048,
    PRE_4096
  } Prescaler deriving(Bits, Eq, FShow);

  typedef enum {
    PAR_NONE,  /* No parity */
    PAR_NONE1, /* Also no parity */
    PAR_EVEN,  /* Even parity */
    PAR_ODD    /* Odd parity */
  } ParityMode deriving(Bits, Eq, FShow);

  typedef struct {
    Prescaler prescaler;
    ParityMode parity_mode;
    Bool cts_enable;
    Bool rts_enable;
    UInt#(12) baud;
  } UartParams deriving(Bits, FShow);

function UInt#(4) getbits(UartParams p);
  return (p.parity_mode == PAR_NONE || p.parity_mode == PAR_NONE1) ? 10 : 11;
endfunction

typedef enum {
  FLOW_NONE,
  FLOW_CTS_ONLY,
  FLOW_RTS_ONLY,
  FLOW_CTS_RTS
} FlowCon deriving(Bits, Eq, FShow);

interface UartCon_IFC;
  method Action setup(UInt#(32) clkrate, UInt#(32) baudrate, ParityMode parity, 
      FlowCon flow_con);

  method Action send(Bit#(8) data);

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
endinterface

module mkUartCon(UartCon_IFC);

  Reg#(UartParams) rg_params <- mkRegU;  // Operation parameters
  Reg#(UInt#(4)) rg_txbits <- mkRegU;    // TX bit counter
  Reg#(UInt#(4)) rg_rxbits <- mkRegU;    // RX bit counter
  Reg#(UInt#(12)) rg_txbaud <- mkRegU;   // TX baud counter
  Reg#(UInt#(12)) rg_rxbaud <- mkRegU;   // RX baud counter
  Reg#(Bit#(1)) rg_cts_sync <- mkRegU;   // CTS synchronizer
  Reg#(Bit#(5)) rg_rxd_sync <- mkRegU;   // RX data synchronizer
  Reg#(Bit#(10)) rg_rxd <- mkRegU;       // RX data register
  Reg#(Bit#(11)) rg_txd <- mkRegU;       // TX data register
  Reg#(Bool) rg_rtr <- mkReg(False);     // RX RTR
  Reg#(Bool) rg_cts <- mkRegU;           // TX CTS

  FIFOF#(Bit#(8)) tx_fifo <- mkFIFOF;

  // Clock dividers
  Reg#(Bit#(12)) rg_clk_div <- mkReg(0);
  Reg#(Bit#(12)) rg_clk_div_ff <- mkReg(0);

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

  // Take DUT TX data
  mkAutoFSM(seq
    while(True) seq
      // Prepare for RX
      action
        rg_rxbaud <= rg_params.baud >> 1;
        rg_rxbits <= getbits(rg_params);
      endaction
        
      // Wait for the start bit
      action
        await(rg_rxd_sync[3:0] == 4'b0011);
      endaction
        
      // RX all bits
      while(rg_rxbits > 0) seq
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
      
      // Store RX data and check parity
      action
        Bit#(8) data;
        Bool perr = False;
        Bool ferr = False;
        if ((rg_params.parity_mode == PAR_NONE) || (rg_params.parity_mode == PAR_NONE1)) begin
          data = rg_rxd[8:1];
          ferr = unpack(~rg_rxd[9]);
        end
        else begin
          data = rg_rxd[7:0];
          perr = unpack(reduceXor(rg_rxd[8:0]));
          if (rg_params.parity_mode == PAR_ODD) perr = !perr;
          ferr = unpack(~rg_rxd[9]);
        end
        $write("%c", data);
        if (perr) $write(" ** Parity Error");
        if (ferr) $write(" ** Frame Error");
        if (perr || ferr) $write("\n");
      endaction
    endseq
  endseq);

  // Send DUT RX data
  mkAutoFSM(seq
    while(True) seq
      rg_txd <= 1;

      await(tx_fifo.notEmpty && rg_cts);
      
      // Prepare for TX
      action
        Bit#(1) p = 1;
        if (rg_params.parity_mode == PAR_ODD || rg_params.parity_mode == PAR_EVEN) begin
          p = reduceXor(tx_fifo.first);
          if (rg_params.parity_mode == PAR_ODD) p = ~p;
        end
        rg_txd <= {1'b1, p, tx_fifo.first, 1'b0};
        tx_fifo.deq;
        rg_txbits <= getbits(rg_params);
        rg_txbaud <= rg_params.baud;
      endaction
      
      // TX all bits
      while (rg_txbits > 0) seq
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

  // =====================================================================
  // Interface

  method Action setup(UInt#(32) clkrate, UInt#(32) baudrate, ParityMode parity, 
      FlowCon flow_con);
    UartParams ret = unpack(0);

    UInt#(12) i = 0; // BAUD rate divisor
    Bit#(3) p = 0; // initial prsc = CLK/2

    i = truncate(clkrate / (2*baudrate));
    
    // find baud prescaler (12-bit wide))
    while (i >= 12'hfff) begin
      if ((p == 2) || (p == 4))
        i = i >> 3;
      else
        i = i >> 1;
      p = p + 1;
    end

    ret.prescaler = unpack(p);
    ret.baud = i-1;
    ret.parity_mode = parity;
    ret.cts_enable = False;
    ret.rts_enable = False;
    case (flow_con)
      FLOW_CTS_ONLY: ret.cts_enable = True;
      FLOW_RTS_ONLY: ret.rts_enable = True;
      FLOW_CTS_RTS: begin
        ret.cts_enable = True;
        ret.rts_enable = True;
      end  
    endcase

    action
      rg_params <= ret;
      $display("================================================================");
      $display("UART Console Model Configuration:");
      $display("System clock: %dHz", clkrate);
      $display("Requested parameters:");
      $display("\tbaud: %d", baudrate);
      $display("\tparity: ", fshow(parity));
      $display("\tflow control: ", fshow(flow_con));
      $display("Parameters applied:");
      $display("\t", fshow(ret));
      $display("================================================================");
    endaction
  endmethod

  method Action send(Bit#(8) data);
    tx_fifo.enq(data);
  endmethod

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
    //return rg_params.rts_enable ? pack(!rg_rtr) : 0;
    // Model is always ready to receive
    return 0;
  endmethod
endmodule

endpackage