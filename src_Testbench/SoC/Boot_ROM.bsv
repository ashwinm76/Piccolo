// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package Boot_ROM;

// ================================================================
// This package implements a slave IP that is a RISC-V boot ROM of
// 1024 32b locations.
// - There are two ports, for Imem and Dmem accesses
// - All accesses are assumed to be reads; don't send writes!
// - Assumes all reads are 4-byte aligned requests for 4-bytes

// ================================================================

export Boot_ROM_IFC (..), mkBoot_ROM;

// ================================================================
// BSV library imports

import ConfigReg :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut     :: *;
import ClientServer::*;
import FIFOF      :: *;

// ================================================================
// Project imports

// ================================================================
// Include the auto-generated BSV-include file with the ROM function

`ifdef RV32
`include  "fn_read_ROM_RV32.bsvi"
`endif

`ifdef RV64
`include  "fn_read_ROM_RV64.bsvi"
`endif

// ================================================================
// Interface

interface Boot_ROM_IFC;
  // read requests
  interface Server#(Bit#(32), Bit#(32)) imem;
  interface Server#(Bit#(32), Bit#(32)) dmem;
endinterface

// ================================================================

(* synthesize *)
module mkBoot_ROM(Boot_ROM_IFC);

  // Verbosity: 0: quiet; 1: reads
  Integer verbosity = 0;

  // Request and response FIFOs
  FIFOF#(Bit#(32)) f_imem_req <- mkFIFOF;
  FIFOF#(Bit#(32)) f_imem_resp <- mkFIFOF;
  FIFOF#(Bit#(32)) f_dmem_req <- mkFIFOF;
  FIFOF#(Bit#(32)) f_dmem_resp <- mkFIFOF;

  // ----------------

  function Bool fn_addr_is_aligned(Bit#(32) addr);
    return (addr[1:0] == 2'b_00);
  endfunction

  function Action read_rom(Bool imem_not_dmem);
  action
    Bit#(32) addr;
    Bit#(32) data = 0;
    if (imem_not_dmem) begin
      addr = f_imem_req.first;
      f_imem_req.deq;
    end
    else begin
      addr = f_dmem_req.first;
      f_dmem_req.deq;
    end
    
    if (!fn_addr_is_aligned(addr)) begin
      $display("%0d: ERROR: Boot_ROM.rl_process_rd_req: unraligned addr",  cur_cycle);
      $display("    ", addr);
    end
    else if (addr[2:0] == 3'b0)
      data = fn_read_ROM_0(addr);
    else  // addr[2:0] == 3'b1_00))
      data = fn_read_ROM_4(addr);
    
    if (imem_not_dmem)
      f_imem_resp.enq(data);
    else
      f_dmem_resp.enq(data);

    if (verbosity > 0) begin
      $display("%0d: Boot_ROM.rl_process_rd_req: ", cur_cycle);
      $display("        0x%0h", addr);
      $display("     => 0x%0h", data);
    end
  endaction
  endfunction

  // ================================================================
  // BEHAVIOR

  // ----------------------------------------------------------------
  // Handle read requests

  rule rl_process_rd_req(f_imem_req.notEmpty || f_dmem_req.notEmpty);
    read_rom(f_imem_req.notEmpty);
  endrule

  // ================================================================
  // INTERFACE

  interface imem = toGPServer(f_imem_req, f_imem_resp);
  interface dmem = toGPServer(f_dmem_req, f_dmem_resp);
endmodule

// ================================================================

endpackage
