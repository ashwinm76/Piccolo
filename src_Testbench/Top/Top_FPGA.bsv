// Copyright (c) 2013-2020 Bluespec, Inc. All Rights Reserved.
// Copyright (c) 2022 Ashwin Menon. All Rights Reserved
// Modified to make it the rv_system0 design.

package Top_FPGA;

// ================================================================
// mkTop_FPGA is the top-level system for simulation.

// ================================================================
// BSV lib imports

import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import StmtFSM      :: *;
import List         :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;

// ================================================================
// Project imports

import ISA_Decls      :: *;
import TV_Info        :: *;
import SoC_Top        :: *;
import Mem_Controller :: *;
import Mem_Model      :: *;
import Uart_TB_Model  :: *;
import Fabric_Defs    :: *;
import PLIC           :: *;

import C_Imports        :: *;
import External_Control :: *;

(* synthesize *)
module mkRAM(Mem_Model_IFC#(`RAM_SIZE));
  Mem_Model_IFC#(`RAM_SIZE) m <- mkMem_Model;
  return m;
endmodule

// ================================================================
// Top-level module.
// Instantiates the SoC.
// Instantiates a memory model.

interface Top_FPGA_IFC;
  // GPIO Inputs
  (* always_ready, always_enabled, prefix = "" *)
  method Action m_gpio_inputs (Bit#(20) gpio_inputs);

  // GPIO Outputs
  (* always_ready *)
  method Bit#(28) gpio_outputs;

  // UART TX signals
  (* always_ready *)
  method Bit#(1) txd_o;

  // UART RX signals
  (* always_ready, always_enabled, prefix = "" *)
  method Action m_rxd_i(Bit#(1) rxd_i);

`ifndef SYNTH
  // Set core's verbosity
   method Action  set_verbosity (Bit #(4)  verbosity, Bit #(64)  logdelay);

  // Catch-all status; return-value can identify the origin (0 = none)
  (* always_ready *)
  method Bit #(8) status;

  // For ISA tests: watch memory writes to <tohost> addr
  method Action set_watch_tohost (Bool  watch_tohost, Fabric_Addr  tohost_addr);
`endif
endinterface

(* synthesize *)
module mkTop_FPGA(Top_FPGA_IFC) ;

  SoC_Top_IFC    soc_top   <- mkSoC_Top;
  Mem_Model_IFC#(`RAM_SIZE)  mem_model <- mkRAM;

  // Connect SoC to raw memory
  let memCnx <- mkConnection (soc_top.to_raw_mem, mem_model.mem_server);

  // ================================================================
  // BEHAVIOR

  rule rl_uart_connections;
    soc_top.cts_i(0);
  endrule

  // ================================================================
  // INTERFACE

  // GPIO Inputs
  method Action m_gpio_inputs (Bit#(20) gpio_inputs);
    soc_top.gpio_input(zeroExtend(gpio_inputs));
  endmethod

  // GPIO Outputs
  method Bit#(28) gpio_outputs;
    return truncate(soc_top.gpio_output);
  endmethod

  // UART TX signals
  interface txd_o = soc_top.txd_o;

  // UART RX signals
  interface m_rxd_i = soc_top.rxd_i;

`ifndef SYNTH
  // Set core's verbosity
  interface set_verbosity = soc_top.set_verbosity;

  // Catch-all status; return-value can identify the origin (0 = none)
  interface status = soc_top.status;

   // For ISA tests: watch memory writes to <tohost> addr
   interface set_watch_tohost = soc_top.set_watch_tohost;
`endif
endmodule

// ================================================================

endpackage: Top_FPGA
