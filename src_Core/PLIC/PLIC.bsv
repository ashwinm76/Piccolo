// Copyright (c) 2019-2020 Bluespec, Inc.  All Rights Reserved

package PLIC;

// ================================================================
// This package implements a PLIC (Platform-Level Interrupt Controller)
// conforming to the RISC-V PLIC standard.
// It is parameterized for:
//   - # of sources
//   - # of targets
//   - # of priorities
//
// ================================================================
// Bluespec lib imports

import  ConfigReg    :: *;
import  Vector       :: *;
import  FIFOF        :: *;
import  ClientServer :: *;
import  Assert       :: *;

// ----------------
// BSV additional libs

import  Cur_Cycle  :: *;
import  GetPut_Aux :: *;
import  Semi_FIFOF :: *;

// ================================================================
// Project imports
`ifdef NO_FABRIC_PLIC
// Near peripheral access interfaces
import Near_Reg_IFC :: *;
typedef 32 Plic_Wd_Addr;
typedef 32 Plic_Wd_Data;
`else
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import Fabric_Defs  :: *;    // for Wd_Id, Wd_Addr, Wd_Data, Wd_User
`endif

// ================================================================
// Change bitwidth without requiring < or > constraints.

function Bit#(m) changeWidth(Bit#(n) x);
  Bit#(TAdd#(m, n)) y = zeroExtend(x);
  Bit#(m) z = y[valueOf(m)-1 : 0];
  return z;
endfunction

// ================================================================
// Maximum supported sources, targets, ...

// Max 1024 sources (source 0 is reserved for 'no interrupt')
typedef 10 T_wd_source_id;
// Max 32 targets
typedef 5 T_wd_target_id;

// ================================================================
// Interfaces

// ----------------
// Individual source interface

interface PLIC_Source_IFC;
  (* always_ready, always_enabled *)
  method Action  m_interrupt_req(Bool set_not_clear);
endinterface

// ----------------
// Individual target interface

interface PLIC_Target_IFC;
  (* always_ready *)
  method Bool  m_eip;    // external interrupt pending
endinterface

// ----------------
// PLIC interface

interface PLIC_IFC#(numeric type t_n_external_sources, numeric type t_n_targets,
    numeric type t_max_priority);
  // Debugging
  method Action set_verbosity(Bit#(4) verbosity);
  method Action show_PLIC_state;

  // Reset
  interface Server#(Bit#(0), Bit#(0)) server_reset;

  // set_addr_map should be called after this module's reset
  method Action set_addr_map(Bit#(64) addr_base, Bit#(64) addr_lim);

  // Memory-mapped access
`ifdef NO_FABRIC_PLIC
  interface Near_Reg_Slave_IFC#(Plic_Wd_Addr, Plic_Wd_Data) reg_access;
`else
  interface AXI4_Slave_IFC#(Wd_Id, Wd_Addr, Wd_Data, Wd_User) axi4_slave;
`endif

  // sources
  interface Vector#(t_n_external_sources, PLIC_Source_IFC) v_sources;

  // targets EIPs (External Interrupt Pending)
  interface Vector#(t_n_targets, PLIC_Target_IFC) v_targets;
endinterface

// ================================================================
// PLIC module implementation

module mkPLIC (PLIC_IFC#(t_n_external_sources, t_n_targets, t_max_priority))
    // source 0 is reserved for 'no source'
    provisos(Add#(1, t_n_external_sources, t_n_sources),
    Add#(_any_0, TLog#(t_n_sources), T_wd_source_id),
    Add#(_any_1, TLog#(t_n_targets), T_wd_target_id),
    Log#(TAdd#(t_max_priority, 1), t_wd_priority));

  // 0 = quiet; 1 = show PLIC transactions; 2 = also show AXI4 transactions
  Reg#(Bit#(4)) cfg_verbosity <- mkConfigReg(0);

  // Source_Ids and Priorities are read and written over the memory interface
  // and should fit within the data bus width, currently 32 bits.
  staticAssert((valueOf(TLog#(t_n_sources)) <= 32), 
      "PLIC: t_n_sources parameter too large");
  staticAssert((valueOf(TLog#(TAdd#(t_max_priority, 1))) <= 32), 
      "PLIC: t_max_priority parameter too large");

  Integer n_sources = valueOf(t_n_sources);
  Integer n_targets = valueOf(t_n_targets);

  // ----------------
  // Soft reset requests and responses
  FIFOF#(Bit#(0)) f_reset_reqs <- mkFIFOF;
  FIFOF#(Bit#(0)) f_reset_rsps <- mkFIFOF;

  // ----------------
  // Memory-mapped access
  
  // Base and limit addrs for this memory-mapped block.
  Reg#(Bit#(64)) rg_addr_lim <- mkRegU;
  Reg#(Bit#(64)) rg_addr_base <- mkRegU;

`ifdef NO_FABRIC_PLIC
  FIFOF#(Near_Reg_Rd_Req#(Plic_Wd_Addr)) f_near_reg_rd_req <- mkFIFOF;
  FIFOF#(Near_Reg_Rd_Resp#(Plic_Wd_Data)) f_near_reg_rd_resp <- mkFIFOF;
  FIFOF#(Near_Reg_Wr_Req#(Plic_Wd_Addr, Plic_Wd_Data)) f_near_reg_wr_req <- mkFIFOF;
  FIFOF#(Near_Reg_Wr_Resp) f_near_reg_wr_resp <- mkFIFOF;
  let resp_okay = RESP_OK;
  let resp_decerr = RESP_DECERR;
  let resp_slverr = RESP_ERR;
`else
  // Connector to AXI4 fabric
  AXI4_Slave_Xactor_IFC#(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave_xactor <- 
      mkAXI4_Slave_Xactor;
  let resp_okay = axi4_resp_okay;
  let resp_decerr = axi4_resp_decerr;
  let resp_slverr = axi4_resp_slverr;
`endif

  // ----------------
  // Per-interrupt source state

  // Interrupt pending from source
  Vector#(t_n_sources, Reg#(Bool)) vrg_source_ip <- 
      replicateM(mkConfigReg(False));
  // Interrupt claimed and being serviced by a hart
  Vector#(t_n_sources, Reg#(Bool)) vrg_source_busy <- replicateM(mkReg(False));
  // Priority for this source
  Vector#(t_n_sources, Reg#(Bit#(t_wd_priority))) vrg_source_prio <- 
      replicateM(mkReg(0));

  // ----------------
  // Per-target hart context state

  // Threshold: interrupts at or below threshold should be masked out for target
  Vector#(t_n_targets, Reg#(Bit#(t_wd_priority))) vrg_target_threshold <- 
      replicateM(mkReg('1));

  // ----------------
  // Per-target, per-source state

  // Interrupt enables from source to target
  Vector#(t_n_targets, Vector#(t_n_sources, Reg#(Bool))) vvrg_ie <- 
      replicateM(replicateM(mkReg(False)));

  // ================================================================
  // Compute outputs for each target (combinational)

  function Tuple2#(Bit#(t_wd_priority), Bit#(TLog#(t_n_sources)))
  fn_target_max_prio_and_max_id(Bit#(T_wd_target_id) target_id);
    Bit#(t_wd_priority) max_prio = 0;
    Bit#(TLog#(t_n_sources)) max_id = 0;

    // Note: source_ids begin at 1, not 0.
    for (Integer source_id = 1; source_id < n_sources; source_id = source_id + 1)
      if (vrg_source_ip[source_id] &&
          (vrg_source_prio[source_id] > max_prio) &&
          (vvrg_ie[target_id][source_id])) begin
        max_id = fromInteger(source_id);
        max_prio = vrg_source_prio[source_id];
      end
    // Assert: if any interrupt is pending (max_id > 0), then prio > 0
    return tuple2(max_prio, max_id);
  endfunction

  function Action fa_show_PLIC_state;
  action
    $display("----------------");
    $write("Src IPs  :");
    for (Integer source_id = 0; source_id < n_sources; source_id = source_id + 1)
      $write(" %0d", pack(vrg_source_ip[source_id]));
    $display("");

    $write("Src Prios:");
    for (Integer source_id = 0; source_id < n_sources; source_id = source_id + 1)
      $write(" %0d", vrg_source_prio[source_id]);
    $display("");

    $write("Src busy :");
    for (Integer source_id = 0; source_id < n_sources; source_id = source_id + 1)
      $write(" %0d", pack(vrg_source_busy[source_id]));
    $display("");

    for (Integer target_id = 0; target_id < n_targets; target_id = target_id + 1) begin
      $write("T %0d IEs  :", target_id);
      for (Integer source_id = 0; source_id < n_sources; source_id = source_id + 1)
        $write(" %0d", vvrg_ie[target_id][source_id]);
      match { .max_prio, .max_id } = fn_target_max_prio_and_max_id(
          fromInteger(target_id));
      $display(
          " MaxPri %0d, Thresh %0d, MaxId %0d", max_prio, 
          vrg_target_threshold[target_id], max_id);
    end
  endaction
  endfunction

  // ================================================================
  // Soft reset

  rule rl_reset;
    if (cfg_verbosity > 0)
      $display("%0d: PLIC.rl_reset", cur_cycle);

    let x <- pop(f_reset_reqs);

    for (Integer source_id = 0; source_id < n_sources; source_id = source_id + 1) begin
      vrg_source_ip[source_id] <= False;
      vrg_source_busy[source_id] <= False;
      vrg_source_prio[source_id] <= 0;
    end

    for (Integer target_id = 0; target_id < n_targets; target_id = target_id + 1) begin
      // Mask all interrupts with highest threshold
      vrg_target_threshold[target_id] <= '1;
    end

    for (Integer target_id = 0; target_id < n_targets; target_id = target_id + 1)
      for (Integer source_id = 0; source_id < n_sources; source_id = source_id + 1)
        vvrg_ie[target_id][source_id] <= False;

`ifndef NO_FABRIC_PLIC
    slave_xactor.reset;
`else
    f_near_reg_rd_req.clear;
    f_near_reg_rd_resp.clear;
    f_near_reg_wr_req.clear;
    f_near_reg_wr_resp.clear;
`endif
    f_reset_rsps.enq(?);
  endrule

  // ================================================================
  // Bus interface for reading/writing control/status regs
  // Relative-address map is same as 'SiFive U54-MC Core Complex Manual v1p0'.
  // Accesses are 4-bytes wide, even though bus may be 64b wide.

  // ----------------------------------------------------------------
  // Handle memory-mapped read requests

  rule rl_process_rd_req(!f_reset_reqs.notEmpty
`ifdef NO_FABRIC_PLIC
      && f_near_reg_rd_req.notEmpty
`endif
  );
`ifdef NO_FABRIC_PLIC
    let rda = f_near_reg_rd_req.first;
    f_near_reg_rd_req.deq;
`else
    let rda <- pop_o(slave_xactor.o_rd_addr);
`endif
    if (cfg_verbosity > 1) begin
      $display("%0d: PLIC.rl_process_rd_req:", cur_cycle);
      $display("    ", fshow(rda));
    end

    let addr_offset = rda.araddr - truncate(rg_addr_base);
    Bit#(32) rdata = 0;
    let rresp = resp_okay;

    if (rda.araddr < truncate(rg_addr_base)) begin
      // Technically this should not happen: the fabric should
      // never have delivered such an addr to this IP.
      $display ("%0d: ERROR: PLIC.rl_process_rd_req: bad addr", cur_cycle);
      $display ("            ", fshow(rda));
      rresp = resp_decerr;
    end
    // Source Priority 
    else if (addr_offset < 'h1000) begin
      Bit#(T_wd_source_id) source_id = truncate(addr_offset[11:2]);
      if ((0 < source_id) && (source_id <= fromInteger(n_sources - 1))) begin
        rdata = changeWidth(vrg_source_prio[source_id]);

        if (cfg_verbosity > 0)
          $display("%0d: PLIC.rl_process_rd_req: reading Source Priority: source %0d = 0x%0h",
                   cur_cycle, source_id, rdata);
      end
      else
        rresp = resp_slverr;
    end
    // Source IPs (interrupt pending).
    // Return 32 consecutive IP bits starting with addr.
    else if (('h1000 <= addr_offset) && (addr_offset < 'h2000)) begin
      Bit#(T_wd_source_id) source_id_base = truncate({ addr_offset[11:0], 5'h0 });

      function Bool fn_ip_source_id(Integer source_id_offset);
        let source_id = source_id_base + fromInteger(source_id_offset);
        Bool ip_source_id = ((source_id <= fromInteger(n_sources - 1)) ?
            vrg_source_ip[source_id] : False);
        return ip_source_id;
      endfunction

      if (source_id_base <= fromInteger(n_sources - 1)) begin
        Bit#(32) v_ip = pack(genWith(fn_ip_source_id));
        rdata = changeWidth(v_ip);

        if (cfg_verbosity > 0)
          $display("%0d: PLIC.rl_process_rd_req: reading Intr Pending 32 bits from source %0d = 0x%0h",
                   cur_cycle, source_id_base, rdata);
      end
      else
        rresp = resp_slverr;
    end
    // Source IEs (interrupt enables) for a target
    // Return 32 consecutive IE bits starting with addr.
    // Target 0 addrs: 2000-207F, Target 1 addrs: 2080-20FF, ...
    else if (('h2000 <= addr_offset) && (addr_offset < 'h3000)) begin
      Bit#(T_wd_target_id) target_id = truncate(addr_offset[11:7]);
      Bit#(T_wd_source_id) source_id_base = truncate({ addr_offset[6:0], 5'h0 });

      function Bool fn_ie_source_id(Integer source_id_offset);
        let source_id = fromInteger(source_id_offset) + source_id_base;
        return ((source_id <= fromInteger (n_sources - 1)) ?
            vvrg_ie[target_id][source_id] : False);
      endfunction

      if ((source_id_base <= fromInteger(n_sources - 1)) &&
          (target_id <= fromInteger(n_targets - 1))) begin
        Bit#(32) v_ie = pack(genWith(fn_ie_source_id));
        rdata = changeWidth(v_ie);

        if (cfg_verbosity > 0)
          $display("%0d: PLIC.rl_process_rd_req: reading Intr Enable 32 bits from source %0d = 0x%0h",
                   cur_cycle, source_id_base, rdata);
      end
      else
        rresp = resp_slverr;
    end
    // Target threshold
    else if ((addr_offset [31:0] & 32'hFFFF_0FFF) == 32'h0020_0000) begin
      Bit#(T_wd_target_id) target_id = truncate(addr_offset[20:12]);
      if (target_id <= fromInteger(n_targets - 1)) begin
        rdata = changeWidth(vrg_target_threshold[target_id]);

        if (cfg_verbosity > 0)
          $display("%0d: PLIC.rl_process_rd_req: reading Threshold for target %0d = 0x%0h",
                   cur_cycle, target_id, rdata);
      end
      else
        rresp = resp_slverr;
    end
    // Interrupt service claim by target
    else if ((addr_offset [31:0] & 32'hFFFF_0FFF) == 32'h0020_0004) begin
      Bit#(T_wd_target_id) target_id = truncate(addr_offset [20:12]);
      match { .max_prio, .max_id } = fn_target_max_prio_and_max_id(target_id);
      Bool eip = (max_prio > vrg_target_threshold[target_id]);
      if (target_id <= fromInteger(n_targets - 1)) begin
        if (max_id != 0) begin
          vrg_source_ip[max_id] <= False;
          vrg_source_busy[max_id] <= True;
          rdata = changeWidth(max_id);
        end

        if (cfg_verbosity > 0)
          $display("%0d: PLIC.rl_process_rd_req: reading Claim for target %0d = 0x%0h",
                   cur_cycle, target_id, rdata);
      end
      else
        rresp = resp_slverr;
    end
    else
      rresp = resp_slverr;

    if (rresp != resp_okay) begin
      $display("%0d: ERROR: PLIC.rl_process_rd_req: unrecognized addr", cur_cycle);
      $display("            ", fshow(rda));
    end

    // Send read-response to bus
`ifdef NO_FABRIC_PLIC
    let rdr = Near_Reg_Rd_Resp {
      rresp: rresp,
      rdata: rdata};
    f_near_reg_rd_resp.enq(rdr);
`else
    Bit#(64) rd = zeroExtend(rdata);
    if ((valueOf(Wd_Data) == 64) && ((addr_offset & 'h7) == 'h4))
      rd = { rdata[31:0], 32'h0 };
    Fabric_Data x = truncate(rd);
    let rdr = AXI4_Rd_Data {
        rid: rda.arid,
        rdata: x,
        rresp: rresp,
        rlast: True,
        ruser: rda.aruser};
    slave_xactor.i_rd_data.enq(rdr);
`endif

    if (cfg_verbosity > 1) begin
      $display("%0d: PLIC.rl_process_rd_req", cur_cycle);
      $display("            ", fshow(rda));
      $display("            ", fshow(rdr));
    end
  endrule: rl_process_rd_req

  // ----------------------------------------------------------------
  // Handle memory-mapped write requests

  // Ad hoc order
  (* descending_urgency = "rl_process_rd_req, rl_process_wr_req" *)

  rule rl_process_wr_req(!f_reset_reqs.notEmpty
`ifdef NO_FABRIC_PLIC
      && f_near_reg_wr_req.notEmpty
`endif
  );
`ifdef NO_FABRIC_PLIC
    let wra = f_near_reg_wr_req.first;
    f_near_reg_wr_req.deq;
    let wrd = wra.wdata;
`else
    let wra <- pop_o(slave_xactor.o_wr_addr);
    let wrd <- pop_o(slave_xactor.o_wr_data);
`endif
    if (cfg_verbosity > 1) begin
      $display("%0d: PLIC.rl_process_wr_req", cur_cycle);
      $display("    ", fshow(wra));
      $display("    ", fshow(wrd));
    end

    let addr_offset = wra.awaddr - truncate(rg_addr_base);
`ifdef NO_FABRIC_PLIC
    let wdata32 = wrd;
`else
    let wdata32 = (((valueOf(Wd_Data) == 64) && ((addr_offset & 'h7) == 'h4)) ?
        wrd.wdata [63:32] : wrd.wdata [31:0]);
`endif
    let bresp = resp_okay;

    if (wra.awaddr < truncate(rg_addr_base)) begin
      // Technically this should not happen: the fabric should
      // never have delivered such an addr to this IP.
      $display("%0d: ERROR: PLIC.rl_process_wr_req: unrecognized addr", cur_cycle);
      $display("            ", fshow(wra));
      $display("            ", fshow(wrd));
      bresp = resp_decerr;
    end
    // Source priority 
    else if (addr_offset < 'h1000) begin
      Bit#(T_wd_source_id) source_id = truncate(addr_offset[11:2]);
      if ((0 < source_id) && (source_id <= fromInteger(n_sources - 1))) begin
        vrg_source_prio[source_id] <= changeWidth(wdata32);

        if (cfg_verbosity > 0)
          $display("%0d: PLIC.rl_process_wr_req: writing Source Priority: source %0d = 0x%0h",
                   cur_cycle, source_id, wdata32);
      end
      else begin
        // Note: write to source_id 0 is error; should it just be ignored?
        bresp = resp_slverr;
      end
    end
    // Source IPs (interrupt pending).
    // Read-only, so ignore write; just check that addr ok.
    else if (('h1000 <= addr_offset) && (addr_offset < 'h2000)) begin
      Bit#(T_wd_source_id) source_id_base = truncate({ addr_offset[11:0], 5'h0 });

      if (source_id_base <= fromInteger(n_sources - 1)) begin
        if (cfg_verbosity > 0)
          $display("%0d: PLIC.rl_process_wr_req: Ignoring write to Read-only Intr Pending 32 bits from source %0d",
                    cur_cycle, source_id_base);
      end
      else
        bresp = resp_slverr;
    end
    // Source IEs (interrupt enables) for a target
    // Write 32 consecutive IE bits starting with addr.
    // Target 0 addrs: 2000-207F, Target 1 addrs: 2080-20FF, ...
    else if (('h2000 <= addr_offset) && (addr_offset < 'h3000)) begin
      Bit#(T_wd_target_id) target_id = truncate(addr_offset[11:7]);
      Bit#(T_wd_source_id) source_id_base = truncate({ addr_offset[6:0], 5'h0 });

      if ((source_id_base <= fromInteger(n_sources - 1)) &&
          (target_id <= fromInteger(n_targets - 1))) begin
        for (Bit#(T_wd_source_id) k = 0; k < 32; k = k + 1) begin
          Bit#(T_wd_source_id) source_id = source_id_base + k;
          if (source_id <= fromInteger(n_sources - 1))
            vvrg_ie[target_id][source_id] <= unpack(wdata32[k]);
        end

        if (cfg_verbosity > 0)
          $display("%0d: PLIC.rl_process_wr_req: writing Intr Enable 32 bits for target %0d from source %0d = 0x%0h",
                    cur_cycle, target_id, source_id_base, wdata32);
      end
      else
        bresp = resp_slverr;
    end
    // Target threshold
    else if ((addr_offset [31:0] & 32'hFFFF_0FFF) == 32'h0020_0000) begin
      Bit#(T_wd_target_id) target_id = truncate(addr_offset[20:12]);
      if (target_id <= fromInteger(n_targets - 1)) begin
        vrg_target_threshold[target_id] <= changeWidth(wdata32);

        if (cfg_verbosity > 0)
          $display("%0d: PLIC.rl_process_wr_req: writing threshold for target %0d = 0x%0h",
                    cur_cycle, target_id, wdata32);
      end
      else
        bresp = resp_slverr;
    end
    // Interrupt service completion by target
    // Write data is the source ID to complete. Disabled sources are ignored.
    else if ((addr_offset [31:0] & 32'hFFFF_0FFF) == 32'h0020_0004) begin
      Bit#(T_wd_target_id) target_id = truncate(addr_offset[20:12]);
      Bit#(T_wd_source_id) source_id = truncate(wdata32);

      if (target_id <= fromInteger(n_targets - 1)) begin
        if ((source_id <= fromInteger(n_sources)) &&
            vvrg_ie[target_id][source_id]) begin
          vrg_source_busy[source_id] <= False;
          if (cfg_verbosity > 0)
            $display("%0d: PLIC.rl_process_wr_req: writing completion for target %0d for source 0x%0h",
                      cur_cycle, target_id, source_id);
        end
        else begin
          if (cfg_verbosity > 0)
            $display("%0d: PLIC.rl_process_wr_req: ignoring completion for target %0d for source 0x%0h",
                      cur_cycle, target_id, source_id);
        end
      end
      else
        bresp = resp_slverr;
    end
    else
      bresp = resp_slverr;

    if (bresp != resp_okay) begin
      $display("%0d: ERROR: PLIC.rl_process_wr_req: unrecognized addr", cur_cycle);
      $display("            ", fshow(wra));
      $display("            ", fshow(wrd));
    end

    // Send write-response to bus
`ifdef NO_FABRIC_PLIC
    let wrr = Near_Reg_Wr_Resp { bresp: bresp };
    f_near_reg_wr_resp.enq(wrr);
`else
    let wrr = AXI4_Wr_Resp {
        bid: wra.awid,
        bresp: bresp,
        buser: wra.awuser};
    slave_xactor.i_wr_resp.enq(wrr);
`endif

    if (cfg_verbosity > 1) begin
      $display("%0d: PLIC.rl_process_wr_req", cur_cycle);
      $display("            ", fshow(wra));
      $display("            ", fshow(wrd));
      $display("            ", fshow(wrr));
    end
  endrule: rl_process_wr_req

  // ================================================================
  // Creator of each source interface

  function PLIC_Source_IFC fn_mk_PLIC_Source_IFC(Integer source_id);
    return interface PLIC_Source_IFC;
      method Action m_interrupt_req(Bool set_not_clear);
      action
        if (! vrg_source_busy[source_id + 1]) begin
          vrg_source_ip[source_id + 1] <= set_not_clear;

          if ((cfg_verbosity > 0) && (vrg_source_ip[source_id + 1] != set_not_clear))
            $display("%0d: %m.m_interrupt_req: changing vrg_source_ip [%0d] to %0d",
                     cur_cycle, source_id + 1, pack (set_not_clear));
        end
      endaction
      endmethod
    endinterface;
  endfunction

  // ================================================================
  // Creator of each target interface

  function PLIC_Target_IFC fn_mk_PLIC_Target_IFC(Integer target_id);
    return interface PLIC_Target_IFC;
      method Bool m_eip;    // external interrupt pending
        match { .max_prio, .max_id } = fn_target_max_prio_and_max_id(
            fromInteger(target_id));
        Bool eip = (max_prio > vrg_target_threshold[target_id]);
        return eip;
      endmethod
    endinterface;
  endfunction

  // ================================================================
  // INTERFACE

  // Debugging
  method Action set_verbosity(Bit#(4) verbosity);
    cfg_verbosity <= verbosity;
  endmethod

  method Action show_PLIC_state;
    fa_show_PLIC_state;
  endmethod

  // Reset
  interface server_reset = toGPServer(f_reset_reqs, f_reset_rsps);

  // set_addr_map should be called after this module's reset
  method Action set_addr_map(Bit#(64) addr_base, Bit#(64) addr_lim);
    if (addr_base[1:0] != 0)
      $display("%0d: WARNING: PLIC.set_addr_map: addr_base 0x%0h is not 4-Byte-aligned",
               cur_cycle, addr_base);

    if (addr_lim[1:0] != 0)
      $display("%0d: WARNING: PLIC.set_addr_map: addr_lim 0x%0h is not 4-Byte-aligned",
               cur_cycle, addr_lim);

    rg_addr_base <= addr_base;
    rg_addr_lim <= addr_lim;

    $display("%0d: PLIC.set_addr_map: base 0x%0h limit 0x%0h", 
        cur_cycle, addr_base, addr_lim);
  endmethod

  // Memory-mapped access
`ifdef NO_FABRIC_PLIC
  interface Near_Reg_Slave_IFC reg_access;
    interface reg_rd = toGPServer(f_near_reg_rd_req, f_near_reg_rd_resp);
    interface reg_wr = toGPServer(f_near_reg_wr_req, f_near_reg_wr_resp);
  endinterface
`else
  interface axi4_slave = slave_xactor.axi_side;
`endif

  // sources
  interface v_sources = genWith(fn_mk_PLIC_Source_IFC);

  // targets
  interface v_targets = genWith(fn_mk_PLIC_Target_IFC);
endmodule

// ================================================================

endpackage
