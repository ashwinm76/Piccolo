// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package Mem_Model;

// ================================================================
// A simulation model of external DRAM memory.
// Uses a register file to model memory.

// ================================================================
// BSV library imports

import  FIFOF        :: *;
import  BRAM         :: *;
import  ClientServer :: *;
import  Memory       :: *;
import  GetPut       :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;

// ================================================================
// Project imports

import Mem_Controller :: *;

// ================================================================
// Mem Model interface

interface Mem_Model_IFC#(numeric type size);
  // The read/write interface
  interface  MemoryServer #(Bits_per_Raw_Mem_Addr, Bits_per_Raw_Mem_Word)  mem_server;
endinterface

// ================================================================
// Mem Model implementation

module mkMem_Model (Mem_Model_IFC#(size));
  FIFOF#(MemoryResponse#(Bits_per_Raw_Mem_Word)) f_mem_responses <- mkFIFOF;

  BRAM_Configure cfg = defaultValue;
  cfg.memorySize = valueOf(TDiv#(size, TDiv#(Bits_per_Raw_Mem_Word, 8)));
                   
`ifndef SYNTH
  `ifdef PRELOAD_RAM
    cfg.loadFormat = tagged Hex "Mem.hex";
  `endif
`endif
  
  BRAM1Port#(Raw_Mem_Addr, Raw_Mem_Word) mem <- mkBRAM1Server(cfg);

  rule rl_mem_responses;
    let d <- mem.portA.response.get;
    f_mem_responses.enq(MemoryResponse{ data: d });
  endrule

  // ----------------------------------------------------------------
  // INTERFACE

  interface MemoryServer mem_server;
    interface Put request;
	    method Action put(MemoryRequest#(Bits_per_Raw_Mem_Addr, Bits_per_Raw_Mem_Word) req);
        BRAMRequest#(Raw_Mem_Addr, Raw_Mem_Word) r = BRAMRequest{
          write: req.write,
          responseOnWrite: False,
          address: req.address,
          datain: req.data
        };
        mem.portA.request.put(r);
	    endmethod
    endinterface

    interface Get response = toGet(f_mem_responses);
  endinterface
endmodule

// ================================================================

endpackage
