// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved
// Copyright (c) 2022 Ashwin Menon. All Rights Reserved
// Created using UART_Model.bsv as a template

package GPIO_Model;

// ================================================================
// This package implements a slave IP, a GPIO model.
//
// Bus interface width: This slave IP can be attached to fabrics with
// 32b- or 64b-wide data channels.  The type parameter 'Wd_Data' in
// Fabric_Defs.bsv specifies this.
//
// Some of the 'truncate()'s and 'zeroExtend()'s below are no-ops but
// necessary to satisfy type-checking, to manage these width
// variations.
// ================================================================

export GPIO_IFC (..), mkGPIO;

// ================================================================
// BSV library imports

import  Vector        :: *;
import  FIFOF         :: *;
import  GetPut        :: *;
import  ClientServer  :: *;
import  ConfigReg     :: *;

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
// GPIO registers and their address offsets

Bit #(3)  addr_GPIO_input_lo  = 3'h_0;    // parallel input pins 31:0
Bit #(3)  addr_GPIO_input_hi  = 3'h_1;    // parallel input pins 63:32
Bit #(3)  addr_GPIO_output_lo  = 3'h_2;    // parallel output pins 31:0
Bit #(3)  addr_GPIO_output_hi  = 3'h_3;    // parallel output pins 63:32

// ================================================================
// THIS MODULE's INTERFACE

interface GPIO_IFC;
   // Reset
   interface Server #(Bit #(0), Bit #(0))  server_reset;

   // set_addr_map should be called after this module's reset
   method Action set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim);

   // Main Fabric Reqs/Rsps
   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave;

   // Inputs
   (* always_ready, always_enabled, prefix = "" *)
   method Action m_gpio_inputs (Bit#(64) gpio_inputs); // in

   // Outputs
   (* always_ready *)
   method Bit#(64) gpio_outputs; // out
endinterface

// ================================================================
// Local types and constants

// Module state
typedef enum {STATE_START,
	      STATE_READY
   } Module_State
deriving (Bits, Eq, FShow);

// ================================================================
// Addressing of GPIO registers

// ----------------------------------------------------------------
// GPIO reg addresses should be at stride 4 or 8.

Integer address_stride = 4;
// Integer address_stride = 8;

// ----------------------------------------------------------------
// Split a bus address into (offset, lsbs), based on the address
// stride.

function Tuple2 #(Bit #(64), Bit #(3)) split_addr (Bit #(64) addr);
   Bit #(64) offset = ((address_stride == 4) ? (addr >> 2)           : (addr >> 3));
   Bit #(3)  lsbs   = ((address_stride == 4) ? { 1'b0, addr [1:0] }  : addr [2:0]);

   return tuple2 (offset, lsbs);
endfunction

// ----------------------------------------------------------------
// Extract data from AXI4 byte lanes, based on the AXI4 'strobe'
// (byte-enable) bits.

function Bit #(64) fn_extract_AXI4_data (Bit #(64) data, Bit #(8) strb);
   Bit #(64) result = 0;
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

(* synthesize *)
module mkGPIO (GPIO_IFC);

   Reg #(Bit #(8)) cfg_verbosity <- mkConfigReg (0);

   Reg #(Module_State) rg_state     <- mkReg (STATE_START);

   // These regs represent where this GPIO is placed in the address space.
   Reg #(Fabric_Addr)  rg_addr_base <- mkRegU;
   Reg #(Fabric_Addr)  rg_addr_lim  <- mkRegU;

   FIFOF #(Bit #(0)) f_reset_reqs <- mkFIFOF;
   FIFOF #(Bit #(0)) f_reset_rsps <- mkFIFOF;

   // ----------------
   // Connector to AXI4 fabric

   AXI4_Slave_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave_xactor <- mkAXI4_Slave_Xactor;

   // ----------------
   // These are the GPIO registers

   Reg #(Bit #(32))  rg_output_lo <- mkRegU;       // addr offset 8
   Reg #(Bit #(32))  rg_output_hi <- mkRegU;       // addr offset c

   Wire #(Bit #(64)) w_inputs <- mkWire;

   // ================================================================
   // BEHAVIOR

   // ----------------------------------------------------------------
   // Soft reset (on token in f_reset_reqs)

   rule rl_reset;
      f_reset_reqs.deq;

      rg_output_lo <= 0;
      rg_output_hi <= 0;

      slave_xactor.reset;
      rg_state <= STATE_READY;

      f_reset_rsps.enq (?);

      if (cfg_verbosity != 0)
	 $display ("%0d: GPIO.rl_reset", cur_cycle);
   endrule

   // ----------------------------------------------------------------
   // Handle fabric read requests

   rule rl_process_rd_req (rg_state == STATE_READY);
      let rda <- pop_o (slave_xactor.o_rd_addr);

      let byte_addr = rda.araddr - rg_addr_base;
      match { .offset, .lsbs } = split_addr (zeroExtend (byte_addr));

      Bit #(32)  rdata_word = 0;
      AXI4_Resp rresp      = axi4_resp_okay;

      if ((rda.araddr < rg_addr_base) || (rda.araddr >= rg_addr_lim)) begin
	 $display ("%0d: %m.rl_process_rd_req: ERROR: GPIO addr out of bounds", cur_cycle);
	 $display ("    GPIO base addr 0x%0h  limit addr 0x%0h", rg_addr_base, rg_addr_lim);
	 $display ("    AXI4 request: ", fshow (rda));
	 rresp = axi4_resp_decerr;
      end
      else if (lsbs != 0) begin
	 $display ("%0d: %m.rl_process_rd_req: ERROR: GPIO misaligned addr", cur_cycle);
	 $display ("            ", fshow (rda));
	 rresp = axi4_resp_slverr;
      end
      else if (offset [63:3] != 0) begin
	 $display ("%0d: %m.rl_process_rd_req: ERROR: GPIO unsupported addr", cur_cycle);
	 $display ("            ", fshow (rda));
	 rresp = axi4_resp_decerr;
      end

      // offset 0: INPUT_LO
      else if (offset [2:0] == addr_GPIO_input_lo) begin
	 rdata_word = w_inputs[31:0];
      end
      // offset 4: INPUT_HI
      else if (offset [2:0] == addr_GPIO_input_hi) begin
	 rdata_word = w_inputs[63:32];
      end
      // offset 8: OUTPUT_LO
      else if (offset [2:0] == addr_GPIO_output_lo) begin
	 rdata_word = rg_output_lo;
      end
      // offset c: OUTPUT_HI
      else if (offset [2:0] == addr_GPIO_output_hi) begin
	 rdata_word = rg_output_hi;
      end
      else begin
	 $display ("%0d: %m.rl_process_rd_req: ERROR: GPIO unsupported addr", cur_cycle);
	 $display ("            ", fshow (rda));
	 rresp = axi4_resp_decerr;
      end

      // Align data byte for AXI4 data bus based on fabric-width
      Fabric_Data rdata = zeroExtend (rdata_word);
      if ((valueOf (Wd_Data) == 64) && (byte_addr [2:0] == 3'b100))
	 rdata = rdata << 32;

      // Send read-response to bus
      let rdr = AXI4_Rd_Data {rid:   rda.arid,
			      rdata: rdata,
			      rresp: rresp,
			      rlast: True,
			      ruser: rda.aruser};
      slave_xactor.i_rd_data.enq (rdr);

      if (cfg_verbosity > 1) begin
	 $display ("%0d: %m.rl_process_rd_req", cur_cycle);
	 $display ("            ", fshow (rda));
	 $display ("            ", fshow (rdr));
      end
   endrule

   // ----------------------------------------------------------------
   // Handle fabric write requests

   rule rl_process_wr_req (rg_state == STATE_READY);
      let wra <- pop_o (slave_xactor.o_wr_addr);
      let wrd <- pop_o (slave_xactor.o_wr_data);

      Bit #(64) wdata     = zeroExtend (wrd.wdata);
      Bit #(8)  wstrb     = zeroExtend (wrd.wstrb);
      Bit #(32)  data_word = truncate (fn_extract_AXI4_data (wdata, wstrb));

      let byte_addr = wra.awaddr - rg_addr_base;
      match { .offset, .lsbs } = split_addr (zeroExtend (byte_addr));

      AXI4_Resp bresp = axi4_resp_okay;

      if ((wra.awaddr < rg_addr_base) || (wra.awaddr >= rg_addr_lim)) begin
	 $display ("%0d: %m.rl_process_rd_req: ERROR: GPIO addr out of bounds", cur_cycle);
	 $display ("    GPIO base addr 0x%0h  limit addr 0x%0h", rg_addr_base, rg_addr_lim);
	 $display ("    AXI4 request: ", fshow (wra));
	 bresp = axi4_resp_decerr;
      end
      else if (lsbs != 0) begin
	 $display ("%0d: %m.rl_process_wr_req: ERROR: GPIO misaligned addr", cur_cycle);
	 $display ("            ", fshow (wra));
	 bresp = axi4_resp_slverr;
      end
      else if (offset [63:3] != 0) begin
	 $display ("%0d: %m.rl_process_wr_req: ERROR: GPIO unsupported addr", cur_cycle);
	 $display ("            ", fshow (wra));
	 bresp = axi4_resp_decerr;
      end

      // offset 0: INPUT_LO
      else if (offset [2:0] == addr_GPIO_input_lo) begin
	 $display ("%0d: %m.rl_process_wr_req: ERROR: GPIO write to INPUT_LO", cur_cycle);
      end
      // offset 4: INPUT_HI
      else if (offset [2:0] == addr_GPIO_input_hi) begin
	 $display ("%0d: %m.rl_process_wr_req: ERROR: GPIO write to INPUT_HI", cur_cycle);
      end
      // offset 8: OUTPUT_LO
      else if (offset [2:0] == addr_GPIO_output_lo) begin
	 rg_output_lo <= data_word;
      end
      // offset c: OUTPUT_HI
      else if (offset [2:0] == addr_GPIO_output_hi) begin
	 rg_output_hi <= data_word;
      end
      else begin
	 $display ("%0d: %m.rl_process_wr_req: ERROR: GPIO unsupported addr", cur_cycle);
	 $display ("            ", fshow (wra));
	 $display ("            ", fshow (wrd));
	 bresp = axi4_resp_decerr;
      end

      // Send write-response to bus
      let wrr = AXI4_Wr_Resp {bid:   wra.awid,
			      bresp: bresp,
			      buser: wra.awuser};
      slave_xactor.i_wr_resp.enq (wrr);

      if (cfg_verbosity > 1) begin
	 $display ("%0d: %m.rl_process_wr_req", cur_cycle);
	 $display ("            ", fshow (wra));
	 $display ("            ", fshow (wrd));
	 $display ("            ", fshow (wrr));
      end
   endrule

   // ================================================================
   // INTERFACE

   // Reset
   interface server_reset   = toGPServer (f_reset_reqs, f_reset_rsps);

   // set_addr_map should be called after this module's reset
   method Action  set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim);
      if (addr_base [2:0] != 0)
	 $display ("%0d: WARNING: GPIO.set_addr_map: addr_base 0x%0h is not 8-Byte-aligned",
		   cur_cycle, addr_base);

      if (addr_lim [2:0] != 0)
	 $display ("%0d: WARNING: GPIO.set_addr_map: addr_lim 0x%0h is not 8-Byte-aligned",
		   cur_cycle, addr_lim);

      rg_addr_base <= addr_base;
      rg_addr_lim  <= addr_lim;
   endmethod

   // Main Fabric Reqs/Rsps
   interface  slave = slave_xactor.axi_side;

   // Inputs
   method Action m_gpio_inputs (Bit#(64) gpio_inputs); // in
     w_inputs <= gpio_inputs;
   endmethod
   
   // Outputs
   method Bit#(64) gpio_outputs; // out
     return {rg_output_hi, rg_output_lo};
   endmethod
endmodule

// ================================================================

endpackage
