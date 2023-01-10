// Copyright (c) 2013-2020 Bluespec, Inc. All Rights Reserved.
// Copyright (c) 2022 Ashwin Menon. All Rights Reserved
// Modified to make it the rv_system0 design.

package Top_FPGA_TB;

// ================================================================
// mkTop_FPGA_TB is the top-level system for simulation.

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

import Top_FPGA       :: *;
import Uart_TB_Model  :: *;
import Fabric_Defs    :: *;
import C_Imports        :: *;
import External_Control :: *;

// ================================================================
// Top-level module.
// Instantiates the FPGA Top.

(* synthesize *)
module mkTop_FPGA_TB(Empty);
  Top_FPGA_IFC dut <- mkTop_FPGA;
  UartCon_IFC uart <- mkUartCon;

  Reg #(Bit#(64)) gpio_o <- mkRegU;

  // ================================================================
  // BEHAVIOR

  Reg #(Bool) rg_banner_printed <- mkReg (False);

  // Display a banner
  rule rl_step0 (! rg_banner_printed);
    $display ("================================================================");
    $display ("Bluespec RISC-V standalone system simulation v1.2");
    $display ("Copyright (c) 2017-2019 Bluespec, Inc. All Rights Reserved.");
    $display ("================================================================");

    rg_banner_printed <= True;

`ifndef SYNTH
    // Set CPU verbosity and logdelay (simulation only)
    Bool v1 <- $test$plusargs ("v1");
    Bool v2 <- $test$plusargs ("v2");
    Bit #(4)  verbosity = ((v2 ? 2 : (v1 ? 1 : 0)));
    Bit #(64) logdelay  = 0;    // # of instructions after which to set verbosity
    dut.set_verbosity(verbosity, logdelay);

    // ----------------
    // Load tohost addr from symbol-table file
    Bool watch_tohost <- $test$plusargs ("tohost");
    let tha <- c_get_symbol_val ("tohost");
    Fabric_Addr tohost_addr = truncate (tha);
    $display ("INFO: watch_tohost = %0d, tohost_addr = 0x%0h",
        pack (watch_tohost), tohost_addr);
    dut.set_watch_tohost (watch_tohost, tohost_addr);
`endif

    // ----------------
    // Start timing the simulation
    Bit #(32) cycle_num <- cur_cycle;
    c_start_timing (zeroExtend (cycle_num));

  endrule: rl_step0

  // ================================================================
  // Terminate on any non-zero status
`ifndef SYNTH
  rule rl_terminate (dut.status != 0);
    $display ("%0d: %m:.rl_terminate: soc_top status is 0x%0h (= 0d%0d)",
    cur_cycle, dut.status, dut.status);

    // End timing the simulation
    Bit #(32) cycle_num <- cur_cycle;
    c_end_timing (zeroExtend (cycle_num));
    $finish (0);
  endrule
`endif

  mkAutoFSM(seq
    // Set up the UART console model
    uart.setup(100000000, 57600, PAR_NONE, FLOW_NONE);

    while(True)
      noAction;
  endseq);

  rule rl_gpio_connections;
    dut.m_gpio_inputs(0);
    if (dut.gpio_outputs != gpio_o) begin
      gpio_o <= dut.gpio_outputs;
      $display("%0d: GPIO Output: %h", cur_cycle, dut.gpio_outputs);
    end
  endrule

  rule rl_uart_connections;
    dut.m_rxd_i(uart.txd_o);
    uart.m_cts_i(0);
    uart.m_rxd_i(dut.txd_o);
  endrule

  // Poll terminal input and relay any chars into system console input.
  // Note: rg_console_in_poll is used to poll only every N cycles, whenever it wraps around to 0.

  Reg #(Bit #(12)) rg_console_in_poll <- mkReg (0);

  rule rl_relay_console_in;
    if (rg_console_in_poll == 0) begin
	    Bit #(8) ch <- c_trygetchar (?);
	    if (ch != 0) begin
	      uart.send(ch);
	    end
    end
    rg_console_in_poll <= rg_console_in_poll + 1;
  endrule

  // ================================================================
  // INTERFACE

  //  None (this is top-level)

endmodule

// ================================================================

endpackage: Top_FPGA_TB
