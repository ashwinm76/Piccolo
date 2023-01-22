package Passthru;

// ================================================================
// BSV lib imports

import ConfigReg    :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF    :: *;
import CreditCounter :: *;

// ================================================================
// Project imports

import ISA_Decls    :: *;
import Near_Mem_IFC :: *;
import SoC_Map      :: *;
import AXI4_Types   :: *;
import Fabric_Defs  :: *;

`ifdef NO_FABRIC_PLIC
import Near_Reg_IFC :: *;
import PLIC         :: *;
`endif

`ifdef NO_FABRIC_CLINT
import Near_Reg_IFC :: *;
import Near_Mem_IO_AXI4 :: *;
`endif

// ================================================================
// Exports

export Passthru_IFC(..), mkDMEM_PT, mkIMEM_PT;

// ================================================================
// Passthrough interface

interface Passthru_IFC;
  method Action set_verbosity(Bit#(4) verbosity);

  // Reset request/response
  interface Server#(Token, Token) server_reset;

  // CPU interface: request
  (* always_ready *)
  method Action req(
      CacheOp op,
      Bit #(3) f3,
`ifdef ISA_A
      Bit #(7) amo_funct7,
`endif
      WordXL addr,
      Bit #(64) st_value);

  //// CPU interface: response
  // Response valid?
  (* always_ready *) method Bool valid;
  // req addr for which this is a response
  (* always_ready *) method WordXL addr;
  // rd_val data for LD, LR, AMO, SC success/fail result)
  (* always_ready *) method Bit#(64) word64;
  // Final stored value for ST, SC, AMO
  (* always_ready *) method Bit#(64) st_amo_val;
  // Exception occurred?
  (* always_ready *) method Bool exc;
  // Exception code
  (* always_ready *) method Exc_Code exc_code;

  // Fabric master interface
  interface AXI4_Master_IFC#(Wd_Id, Wd_Addr, Wd_Data, Wd_User) mem_master;

`ifdef NO_FABRIC_BOOTROM
  // Boot ROM client interface
  interface Client#(Bit#(32), Bit#(32)) boot_rom;
`endif

// TODO: this should only be on the DMEM
`ifdef NO_FABRIC_PLIC
  // PLIC near interface
  interface Near_Reg_Master_IFC#(Plic_Wd_Addr, Plic_Wd_Data) plic_reg_access;
`endif

// TODO: this should only be on the DMEM
`ifdef NO_FABRIC_CLINT
  // CLINT near interface
  interface Near_Reg_Master_IFC#(Clint_Wd_Addr, Clint_Wd_Data) clint_reg_access;
`endif
endinterface

// ****************************************************************
// Internal types and constants

typedef enum {
    MODULE_PRERESET,              // After power on reset, before soft reset
    MODULE_RESETTING,             // During reset
    MODULE_READY,                 // Reset done, ready for first request
    MODULE_RUNNING,               // Normal operation
    MODULE_EXCEPTION_RSP,         // On misaligned and access exceptions
    MEM_RD,                       // Read memory from the fabric
    MEM_AWAITING_RD_RSP,          // Wait for the read response
    MEM_RD_RSP,                   // Provide the mem read response
    MEM_ST_AMO_RSP,               // Provide ST/SC/AMO response
    IO_REQ,                       // For memory-mapped I/O requests
    IO_AWAITING_RD_RSP,           // Wait for IO read response
    IO_AWAITING_AMO_RD_RSP,       // Wait for IO AMO read response
    IO_RD_RSP                     // Provide IO-read response
   } Module_State deriving (Bits, Eq, FShow);

// ================================================================
// Check if addr is aligned

function Bool fn_is_aligned(Bit#(3) f3, Bit#(n) addr);
  return ((f3[1:0] == 2'b00)                             // B, BU
    || ((f3[1:0] == 2'b01) && (addr[0] == 1'b0))         // H, HU
    || ((f3[1:0] == 2'b10) && (addr[1:0] == 2'b00))      // W, WU
    || ((f3[1:0] == 2'b11) && (addr[2:0] == 3'b000)));   // D
endfunction

// ================================================================
// Convert RISC-V funct3 code into AXI4_Size code (number of bytes in a beat)

function AXI4_Size fn_funct3_to_AXI4_Size(Bit#(3) funct3);
  Bit#(2) x = funct3[1:0];
  AXI4_Size result;
  if (x == f3_SIZE_B) result = axsize_1;
  else if (x == f3_SIZE_H) result = axsize_2;
  else if (x == f3_SIZE_W) result = axsize_4;
  else /* if (x == f3_SIZE_D) */ result = axsize_8;
  return result;
endfunction

// ================================================================
// Compute address, data and strobe (byte-enables) for writes to fabric

function Tuple4#(Fabric_Addr,    // addr is 32b- or 64b-aligned
    Fabric_Data,    // data is lane-aligned
    Fabric_Strb,    // strobe
    AXI4_Size)      // 8 for 8-byte writes, else 4
  fn_to_fabric_write_fields(Bit#(3) f3,      // RISC-V size code: B/H/W/D
    Bit#(n) addr,    // actual byte addr
    Bit#(64) word64)  // data is in lsbs
    provisos(Add#(_, n, 64));
  // First compute addr, data and strobe for a 64b-wide fabric
  Bit#(8) strobe64 = 0;
  Bit#(3) shift_bytes = addr[2:0];
  Bit#(6) shift_bits = { shift_bytes, 3'b0 };
  Bit#(64) addr64 = zeroExtend(addr);
  AXI4_Size axsize = axsize_128;    // Will be updated in 'case' below

  case (f3[1:0])
    f3_SIZE_B: begin
      word64 = (word64 << shift_bits);
      strobe64 = ('b_1 << shift_bytes);
      axsize = axsize_1;
    end
    f3_SIZE_H: begin
      word64 = (word64 << shift_bits);
      strobe64 = ('b_11 << shift_bytes);
      axsize = axsize_2;
    end
    f3_SIZE_W: begin
      word64 = (word64 << shift_bits);
      strobe64 = ('b_1111 << shift_bytes);
      axsize = axsize_4;
    end
    f3_SIZE_D: begin
      strobe64 = 'b_1111_1111;
      axsize = axsize_8;
    end
  endcase

  // Adjust for 32b fabrics
  if ((valueOf(Wd_Data) == 32) && (addr[2] == 1'b1)) begin
    word64 = { 32'h0, word64[63:32] };
    strobe64 = { 4'h0, strobe64[7:4] };
  end

  // Finally, create fabric addr/data/strobe
  Fabric_Addr fabric_addr = truncate(addr64);
  Fabric_Data fabric_data = truncate(word64);
  Fabric_Strb fabric_strobe = truncate(strobe64);

  return tuple4(fabric_addr, fabric_data, fabric_strobe, axsize);
endfunction: fn_to_fabric_write_fields

// ================================================================
// ALU for AMO ops.
// Returns the value to be stored back to mem.

`ifdef ISA_A
function Tuple2#(Bit#(64), Bit#(64)) fn_amo_op(
    Bit#(3) funct3,    // encodes data size (.W or .D)
    Bit#(7) funct7,    // encodes the AMO op
    WordXL addr,       // lsbs indicate which 32b W in 64b D (.W)
    Bit#(64) ld_val,   // 64b value loaded from mem
    Bit#(64) st_val);  // 64b value from CPU reg Rs2
  Bit#(64) w1 = fn_extract_and_extend_bytes(funct3, addr, ld_val);
  Bit#(64) w2 = st_val;
  Int#(64) i1 = unpack(w1);    // Signed, for signed ops
  Int#(64) i2 = unpack(w2);    // Signed, for signed ops
  if (funct3 == f3_AMO_W) begin
    w1 = zeroExtend(w1[31:0]);
    w2 = zeroExtend(w2[31:0]);
    i1 = unpack(signExtend(w1[31:0]));
    i2 = unpack(signExtend(w2[31:0]));
  end
  Bit#(5) f5 = funct7[6:2];
  // new_st_val is new value to be stored back to mem (w1 op w2)
  Bit#(64) new_st_val = ?;
  case (f5)
    f5_AMO_SWAP: new_st_val = w2;
    f5_AMO_ADD:  new_st_val = pack(i1 + i2);
    f5_AMO_XOR:  new_st_val = w1 ^ w2;
    f5_AMO_AND:  new_st_val = w1 & w2;
    f5_AMO_OR:   new_st_val = w1 | w2;
    f5_AMO_MINU: new_st_val = ((w1 < w2) ? w1 : w2);
    f5_AMO_MAXU: new_st_val = ((w1 > w2) ? w1 : w2);
    f5_AMO_MIN:  new_st_val = ((i1 < i2) ? w1 : w2);
    f5_AMO_MAX:  new_st_val = ((i1 > i2) ? w1 : w2);
  endcase

  if (funct3 == f3_AMO_W)
    new_st_val = zeroExtend(new_st_val[31:0]);

  return tuple2(truncate(pack(i1)), new_st_val);
endfunction: fn_amo_op
`endif

// ================================================================
// The module

(* synthesize *)
module mkPassthru#(parameter Bool dmem_not_imem)(Passthru_IFC);

  String d_or_i = (dmem_not_imem ? "DMEM_PT" : "IMEM_PT");
  Reg#(Bit#(4)) cfg_verbosity <- mkConfigReg(0);
  Reg#(Module_State) rg_state <- mkReg(MODULE_PRERESET);

  // Fabric request/response
  AXI4_Master_Xactor_IFC#(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master_xactor <- 
      mkAXI4_Master_Xactor;

  // Fabric read data
  Reg#(Maybe#(Bit#(64))) rg_rd_data <- mkReg(tagged Invalid);

  // System address map
  SoC_Map_IFC soc_map <- mkSoC_Map;

  // Reset response queue
  FIFOF#(Token) f_reset_reqs <- mkFIFOF;
  FIFOF#(Token) f_reset_rsps <- mkFIFOF;

  // For discarding write-responses
  // Max 15 writes outstanding
  CreditCounter_IFC#(4) ctr_wr_rsps_pending <- mkCreditCounter;

  //// Registers holding incoming request args
  // CACHE_LD, CACHE_ST, CACHE_AMO
  Reg#(CacheOp) rg_op <- mkRegU;
  // rg_f3[1:0] specifies B/H/W/D access size
  Reg#(Bit#(3)) rg_f3 <- mkRegU;
`ifdef ISA_A
  // specifies which kind of AMO op
  Reg#(Bit#(7)) rg_amo_funct7 <- mkRegU;
`endif
  // VA or PA
  Reg#(WordXL) rg_addr <- mkRegU;
  // Store-value for ST, SC, AMO
  Reg#(Bit#(64)) rg_st_amo_val <- mkRegU;

  //// Outputs
  Reg#(Bool) dw_valid <- mkDWire(False);
  Reg#(Bool) dw_exc <- mkDWire(False);
  Reg#(Exc_Code) rg_exc_code <- mkRegU;
  Reg#(Exc_Code) dw_exc_code <- mkDWire(?);
  // Load-value for LOAD/LR/AMO, success/fail for SC
  Reg#(Bit#(64)) rg_ld_val <- mkRegU;
  Reg#(Bit#(64)) dw_output_ld_val <- mkDWire(?);
  // stored value for ST, SC, AMO (for verification only)
  Reg#(Bit#(64)) dw_output_st_amo_val <- mkDWire(?);

`ifdef NO_FABRIC_BOOTROM
  FIFOF#(Bit#(32)) f_boot_rom_req <- mkFIFOF;
  FIFOF#(Bit#(32)) f_boot_rom_resp <- mkFIFOF;
`endif

// TODO: this should only be on the DMEM
`ifdef NO_FABRIC_PLIC
  FIFOF#(Near_Reg_Rd_Req#(Plic_Wd_Addr)) f_plic_rd_req <- mkFIFOF;
  FIFOF#(Near_Reg_Rd_Resp#(Plic_Wd_Data)) f_plic_rd_resp <- mkFIFOF;
  FIFOF#(Near_Reg_Wr_Req#(Plic_Wd_Addr, Plic_Wd_Data)) f_plic_wr_req <- mkFIFOF;
  FIFOF#(Near_Reg_Wr_Resp) f_plic_wr_resp <- mkFIFOF;
`endif

// TODO: this should only be on the DMEM
`ifdef NO_FABRIC_CLINT
  FIFOF#(Near_Reg_Rd_Req#(Clint_Wd_Addr)) f_clint_rd_req <- mkFIFOF;
  FIFOF#(Near_Reg_Rd_Resp#(Clint_Wd_Data)) f_clint_rd_resp <- mkFIFOF;
  FIFOF#(Near_Reg_Wr_Req#(Clint_Wd_Addr, Clint_Wd_Data)) f_clint_wr_req <- mkFIFOF;
  FIFOF#(Near_Reg_Wr_Resp) f_clint_wr_resp <- mkFIFOF;
`endif

`ifdef ISA_A
  // Reservation regs for AMO LR/SC (Load-Reserved/Store-Conditional)
  Reg#(Bool) rg_lrsc_valid <- mkReg(False);
  Reg#(PA) rg_lrsc_pa <- mkRegU;    // Phys. address for an active LR
`endif

  // Abbreviations testing for LR and SC (avoids ifdef clutter later)
`ifdef ISA_A
  Bool is_AMO = (rg_op == CACHE_AMO);
  Bool is_AMO_LR = ((rg_op == CACHE_AMO) && (rg_amo_funct7[6:2] == f5_AMO_LR));
  Bool is_AMO_SC = ((rg_op == CACHE_AMO) && (rg_amo_funct7[6:2] == f5_AMO_SC));
`else
  Bool is_AMO = False;
  Bool is_AMO_LR = False;
  Bool is_AMO_SC = False;
`endif

  Exc_Code access_exc_code = fn_access_exc_code(dmem_not_imem, 
      ((rg_op == CACHE_LD) || is_AMO_LR));

  // ----------------------------------------------------------------
  // Functions to drive read-responses (outputs)

  // Memory-read responses
  function Action fa_drive_mem_rsp(Bit#(3) f3, Addr addr, Bit#(64) ld_val, 
      Bit#(64) st_amo_val);
  action
    dw_valid <= True;
    // Value loaded into rd (LOAD, LR, AMO, SC success/fail result)
    dw_output_ld_val <= (is_AMO_SC ? ld_val : fn_extract_and_extend_bytes(f3, 
        addr, ld_val));
    // Value stored into mem (STORE, SC, AMO final value stored)
    dw_output_st_amo_val <= st_amo_val;
    if (cfg_verbosity > 1)
      $display("%0d: %s.drive_mem_rsp: addr 0x%0h ld_val 0x%0h st_amo_val 0x%0h",
          cur_cycle, d_or_i, addr, ld_val, st_amo_val);
  endaction
  endfunction

  // IO-read responses
  function Action fa_drive_IO_read_rsp(Bit#(3) f3, Addr addr, Bit#(64) ld_val);
  action
    dw_valid <= True;
    // Value loaded into rd (LOAD, LR, AMO, SC success/fail result)
    dw_output_ld_val <= ld_val;
    if (cfg_verbosity > 1)
      $display("%0d: %s.drive_IO_read_rsp: addr 0x%0h ld_val 0x%0h", cur_cycle,
          d_or_i, addr, ld_val);
  endaction
  endfunction

  // Send a read-request into the fabric
  function Action fa_fabric_send_read_req(Fabric_Addr addr, AXI4_Size size);
  action
    let mem_req_rd_addr = AXI4_Rd_Addr {
        arid: fabric_default_id,
        araddr: addr,
        arlen: 0,           // burst len = arlen+1
        arsize: size,
        arburst: fabric_default_burst,
        arlock: fabric_default_lock,
        arcache: fabric_default_arcache,
        arprot: fabric_default_prot,
        arqos: fabric_default_qos,
        arregion: fabric_default_region,
        aruser: fabric_default_user};

    master_xactor.i_rd_addr.enq(mem_req_rd_addr);

    // Debugging
    if (cfg_verbosity > 1) begin
      $display ("            To fabric: ", fshow(mem_req_rd_addr));
    end
  endaction
  endfunction

  FIFOF#(Tuple3#(Bit#(3), PA, Bit#(64))) f_fabric_write_reqs <- mkFIFOF;

  // Send a write-request into the fabric
  function Action fa_fabric_send_write_req (Bit#(3) f3, PA pa, Bit#(64) st_val);
  action
    f_fabric_write_reqs.enq(tuple3(f3, pa, st_val));
  endaction
  endfunction

  rule rl_fabric_send_write_req;
    match { .f3, .pa, .st_val } <- pop(f_fabric_write_reqs);
    match {
        .fabric_addr,
        .fabric_data,
        .fabric_strb,
        .fabric_size } = fn_to_fabric_write_fields(f3, pa, st_val);

    let mem_req_wr_addr = AXI4_Wr_Addr {
        awid:     fabric_default_id,
        awaddr:   fabric_addr,
        awlen:    0,           // burst len = awlen+1
        awsize:   fabric_size,
        awburst:  fabric_default_burst,
        awlock:   fabric_default_lock,
        awcache:  fabric_default_awcache,
        awprot:   fabric_default_prot,
        awqos:    fabric_default_qos,
        awregion: fabric_default_region,
        awuser:   fabric_default_user };

    let mem_req_wr_data = AXI4_Wr_Data {
        wdata:  fabric_data,
        wstrb:  fabric_strb,
        wlast:  True,
        wuser:  fabric_default_user };

    master_xactor.i_wr_addr.enq(mem_req_wr_addr);
    master_xactor.i_wr_data.enq(mem_req_wr_data);

    // Expect a fabric response
    ctr_wr_rsps_pending.incr;

    // Debugging
    if (cfg_verbosity > 1) begin
      $display ("            To fabric: ", fshow(mem_req_wr_addr));
      $display ("                       ", fshow(mem_req_wr_data));
    end
  endrule

  // ----------------------------------------------------------------
  // BEHAVIOR
  
  /* The Module state machine
    Reset
    -----
    [Start] -> PRERESET
    [!RESETTING] -- (reset req) -> RESETTING
    [RESETTING] -> READY

    Running
    -------
    [*] -- (CPU req) -> RUNNING
    [RUNNING]:
      --(dmem && !is_mem_addr) -> IO_REQ
      --(op==CACHE_LD || is_AMO_LR)
        --(rd data ready)-> MEM_RD_RSP
        --(!rd data ready)-> MEM_RD
      --(op==CACHE_ST || is_AMO_SC)-> MEM_ST_AMO_RSP
      --(op==other AMOs)
        --(!rd data ready)-> MEM_RD
        --(rd data ready)-> MEM_ST_AMO_RSP

    Fabric mem read
    ---------------
    [MEM_RD] --(0 wr resps pending)-> MEM_AWAITING_RD_RSP
    [MEM_AWAITING_RD_RSP] --(read resp OK) -> MODULE_RUNNING
    [MEM_RD_RSP] -> MEM_RD_RSP

    ST/SC/AMO response
    ------------------
    [MEM_ST_AMO_RSP] -> [MEM_ST_AMO_RSP]
    Also IO_REQ

    IO req
    ------
    [IO_REQ]:
      --(op==CACHE_LD || is_AMO_LR) && (0 wr resps pending)-> IO_AWAITING_RD_RSP
      --(op==CACHE_ST)-> MEM_ST_AMO_RSP
      --(is_AMO_SC)-> MEM_ST_AMO_RSP
      --(is_AMO && !is_AMO_LR && !is_AMO_SC)-> IO_AWAITING_AMO_RD_RSP
    [IO_AWAITING_RD_RSP] --(read resp OK)-> IO_RD_RSP
    [IO_AWAITING_AMO_RD_RSP] --(read resp OK) -> IO_RD_RSP
    [IO_RD_RSP] -> IO_RD_RSP

    Exception
    ---------
    [MEM_AWAITING_RD_RSP] --(read resp != OK) -> EXCEPTION_RSP
    [IO_AWAITING_RD_RSP] --(read resp != OK) -> EXCEPTION_RSP
    [IO_AWAITING_AMO_RD_RSP] --(read resp != OK) -> EXCEPTION_RSP
    (CPU req) --(req addr not aligned) -> EXCEPTION_RSP
    [EXCEPTION_RSP] -> [EXCEPTION_RSP]
  */

  // ----------------
  // Reset

  rule rl_start_reset((f_reset_reqs.notEmpty) && (rg_state != MODULE_RESETTING));
    f_reset_reqs.deq;
    rg_state <= MODULE_RESETTING;

`ifdef ISA_A
    rg_lrsc_valid  <= False;
`endif

    master_xactor.reset;
    ctr_wr_rsps_pending.clear;

    if (cfg_verbosity > 1)
      $display("%0d: %s:    => MODULE_RESETTING", cur_cycle, d_or_i);
  endrule

  rule rl_reset(rg_state == MODULE_RESETTING);
    f_reset_rsps.enq(?);
    rg_state <= MODULE_READY;
    if (cfg_verbosity > 1)
      $display("%0d: %s:    => MODULE_READY", cur_cycle, d_or_i);
  endrule

  // ----------------------------------------------------------------
  // This rule provides a response for Mem (non-IO) 
  // requests, if possible, i.e., if LD or AMO_LR, Rd data available.
  // Otherwise, moves to other states that handle Mem reads, 
  // 1-cycle delayed responses for ST and AMO, I/O requests, etc.

  rule rl_mem_immed_rsp(rg_state == MODULE_RUNNING);
    // Print some initial information for debugging
    if (cfg_verbosity > 1)
      $display("%0d: %s: rl_mem_immed_rsp; eaddr %0h", cur_cycle, d_or_i, 
          rg_addr);

    let is_mem_addr = soc_map.m_is_mem_addr(fn_PA_to_Fabric_Addr(rg_addr));

    // Access to non-memory
    if (dmem_not_imem && (!is_mem_addr)) begin
      // IO requests
      rg_state <= IO_REQ;
      if (cfg_verbosity > 1)
        $display ("    => IO_REQ");
    end
    // Memory requests. Note: it's ok that this can go to non-memory space.
    else begin
      // ----------------
      // Memory LD and AMO_LR
      if ((rg_op == CACHE_LD) || is_AMO_LR) begin
        if (isValid(rg_rd_data)) begin
          // Read data ready; drive response
          fa_drive_mem_rsp(rg_f3, rg_addr, rg_rd_data.Valid, 0);

`ifdef ISA_A
          if (is_AMO_LR) begin
            rg_lrsc_valid <= True;
            rg_lrsc_pa <= rg_addr;
            if (cfg_verbosity > 1)
              $display("        AMO LR: reserving PA 0x%0h", rg_addr);
          end
`endif
          if (cfg_verbosity > 1) begin
            $display("        Mem Read ready: addr 0x%0h word64 0x%0h", rg_addr,
                rg_rd_data.Valid);
          end
          rg_state <= MEM_RD_RSP;
        end
        else begin
          // Read data not ready; fabric mem read
          rg_state <= MEM_RD;
          if (cfg_verbosity > 1)
            $display ("        Mem Read not ready: -> MEM_RD.");
        end
      end
      // ----------------
      // Memory ST and AMO SC
      else if ((rg_op == CACHE_ST) || is_AMO_SC) begin
        Bool do_write = True;    // Always True for ST; success/fail for AMO_SC
`ifdef NO_FABRIC_BOOTROM
        do_write = !soc_map.m_is_boot_rom_addr(fn_PA_to_Fabric_Addr(rg_addr));
`endif
`ifdef ISA_A
        // ST: if to an LR/SC reserved address, invalidate the reservation
        if ((rg_op == CACHE_ST) && (rg_addr == rg_lrsc_pa)) begin
          rg_lrsc_valid <= False;
          if (cfg_verbosity > 1)
            $display ("        ST: cancelling LR/SC reservation for PA", rg_addr);
        end
        // AMO_SC
        else if (is_AMO_SC) begin
          // Fail if reservation is not valid, or if not to the reserved addr
          if (!rg_lrsc_valid) begin
            do_write = False;
            if (cfg_verbosity > 1)
              $display ("        AMO SC: fail due to invalid LR/SC reservation");
          end
          else if (rg_lrsc_pa != rg_addr) begin
            do_write = False;
            if (cfg_verbosity > 1)
              $display(
                  "        AMO SC: fail: reserved addr 0x%0h, this address 0x%0h",
                  rg_lrsc_pa, rg_addr);
          end

          // SC result=0 on success, =1 on failure
          Bit#(1) lrsc_result = (do_write ? 1'b0 : 1'b1);

          rg_ld_val <= zeroExtend(lrsc_result);
          rg_lrsc_valid <= False;
          if (cfg_verbosity > 1)
            $display ("        AMO SC result = %0d", lrsc_result);
        end
`endif
        if (do_write) begin
          // ST, or successful SC
          if (cfg_verbosity > 1)
            $display("        Write: eaddr 0x%0h word64 0x%0h", rg_addr,
                rg_st_amo_val);

          // Write data to fabric memory
          fa_fabric_send_write_req(rg_f3, rg_addr, rg_st_amo_val);

          if (cfg_verbosity > 1)
            $display ("        => rl_write_response");
        end
        else begin // do_write == False
          // SC fail
          fa_drive_mem_rsp(rg_f3, rg_addr, 1, 0);
          if (cfg_verbosity > 1)
            $display ("        AMO SC: Fail response for addr 0x%0h", rg_addr);
        end

        // Provide write-response after 1-cycle delay, in case the next 
        // incoming request tries to read from the same SRAM address.
        rg_state <= MEM_ST_AMO_RSP;
      end
`ifdef ISA_A
      // ----------------
      // Remaining AMOs
      else begin
        if (!isValid(rg_rd_data)) begin
          // Rd data not ready; fabric mem read
          rg_state <= MEM_RD;
          if (cfg_verbosity > 1)
              $display ("        AMO Miss: -> MEM_RD.");
        end
        else begin
          Bool do_write = True;    // Always True for ST; success/fail for AMO_SC
`ifdef NO_FABRIC_BOOTROM
          // TODO: should ROM writes trigger an exception?
          do_write = !soc_map.m_is_boot_rom_addr(rg_addr);
`endif
          if (cfg_verbosity > 1) begin
              $display("        AMO: addr 0x%0h amo_f7 0x%0h f3 %0d rs2_val 0x%0h",
                  rg_addr, rg_amo_funct7, rg_f3, rg_st_amo_val);
              $display ("          PA 0x%0h ", rg_addr);
              $display ("          MEM word64 0x%0h, load-result 0x%0h", word64,
                  word64);
          end

          // Do the AMO op on the loaded value and the store value
          match {.new_ld_val, .new_st_val} = fn_amo_op(rg_f3, rg_amo_funct7,
              rg_addr, word64, rg_st_amo_val);

          // Write data to target
          if (do_write)
            fa_fabric_send_write_req(rg_f3, rg_addr, new_st_val);

          if (cfg_verbosity > 1) begin
            $display("          0x%0h  op  0x%0h -> 0x%0h", word64, word64,
                new_st_val);
          end

          // If this is to the LR/SC reserved address, invalidate the reservation
          // TODO: should we invalidate even if to a different
          // addr, since LR/SC pairs are not supposed to have
          // other mem ops between them?
          if (rg_addr == rg_lrsc_pa) begin
            rg_lrsc_valid <= False;
            if (cfg_verbosity > 1)
              $display("        AMO_op: cancelling LR/SC reservation for PA",
                  rg_addr);
          end

          // Provide amo response after 1-cycle delay in case the next incoming 
          // request tries to read from the same address.
          rg_ld_val <= new_ld_val;
          rg_st_amo_val <= new_st_val;
          rg_state <= MEM_ST_AMO_RSP;
        end
      end
`endif
    end
  endrule

  // Read fabric memory
  rule rl_read_fabric_mem((rg_state == MEM_RD) && 
      (ctr_wr_rsps_pending.value == 0));
    if (cfg_verbosity > 1)
      $display ("%0d: %s.rl_read_fabric_mem: ", cur_cycle, d_or_i);

`ifdef NO_FABRIC_BOOTROM
    if (soc_map.m_is_boot_rom_addr(fn_PA_to_Fabric_Addr(rg_addr))) begin
      Bit#(32) addr = truncate(rg_addr) - truncate(soc_map.m_boot_rom_addr_base);
      f_boot_rom_req.enq(addr);
      if (cfg_verbosity > 1)
        $display ("%0d: %s.rl_read_fabric_mem: BOOT ROM RD: %0h", cur_cycle,
            d_or_i, rg_addr);
    end
    else begin
`endif
    Fabric_Addr fabric_addr = fn_PA_to_Fabric_Addr(rg_addr);
`ifdef FABRIC32
    AXI4_Size axi4_size = axsize_4;
`else
    AXI4_Size axi4_size = axsize_8;
    fabric_addr = { fabric_addr[valueOf(Wd_Addr)-1:3], 3'b0 };
`endif
    fa_fabric_send_read_req(fabric_addr, axi4_size);
`ifdef NO_FABRIC_BOOTROM
    end
`endif
    rg_state <= MEM_AWAITING_RD_RSP;
  endrule

function Bool fn_is_fabric_req;
  Bool ret = True;
`ifdef NO_FABRIC_BOOTROM
  ret = ret && !soc_map.m_is_boot_rom_addr(fn_PA_to_Fabric_Addr(rg_addr));
`endif
`ifdef NO_FABRIC_PLIC
  ret = ret && !soc_map.m_is_plic_addr(fn_PA_to_Fabric_Addr(rg_addr));
`endif
`ifdef NO_FABRIC_CLINT
  ret = ret && !soc_map.m_is_near_mem_IO_addr(fn_PA_to_Fabric_Addr(rg_addr));
`endif
  return ret;
endfunction

function Bool fn_is_near_mem_rd_resp_pending;
  Bool ret = False;
`ifdef NO_FABRIC_BOOTROM
  ret = ret || f_boot_rom_resp.notEmpty;
`endif
  return ret;
endfunction

function Bool fn_is_io_rd_resp_pending;
  Bool ret = False;
`ifdef NO_FABRIC_PLIC
  ret = ret || f_plic_rd_resp.notEmpty;
`endif
`ifdef NO_FABRIC_CLINT
  ret = ret || f_clint_rd_resp.notEmpty;
`endif
  return ret;
endfunction

  // Get fabric memory read responses
  rule rl_fabric_mem_rd_rsp(
      rg_state == MEM_AWAITING_RD_RSP &&
      (fn_is_fabric_req || fn_is_near_mem_rd_resp_pending));
`ifdef NO_FABRIC_BOOTROM
    if (f_boot_rom_resp.notEmpty) begin
      if (rg_addr[2:0] == 'b0)
        rg_rd_data <= tagged Valid zeroExtend(f_boot_rom_resp.first);
      else begin
        Bit#(64) dat = {f_boot_rom_resp.first, 32'h0};
        rg_rd_data <= tagged Valid dat;
      end
      if (cfg_verbosity > 2) begin
        $display("%0d: %s.rl_fabric_mem_rd_rsp: BOOT ROM RD", cur_cycle, d_or_i);
        $display("        0x%0h", f_boot_rom_resp.first);
      end
      f_boot_rom_resp.deq;
      rg_state <= MODULE_RUNNING;
    end
    else begin
`endif
    let mem_rsp <- pop_o(master_xactor.o_rd_data);
    if (cfg_verbosity > 2) begin
      $display("%0d: %s.rl_fabric_mem_rd_rsp:", cur_cycle, d_or_i);
      $display("        ", fshow(mem_rsp));
    end

    // Bus errors; remember it, and raise exception
    if (mem_rsp.rresp != axi4_resp_okay) begin
      rg_exc_code <= access_exc_code;
      rg_state <= MODULE_EXCEPTION_RSP;
      if (cfg_verbosity > 1)
        $display("%0d: %s.rl_fabric_mem_rd_rsp: FABRIC_RSP_ERR: raising access exception %0d",
            cur_cycle, d_or_i, access_exc_code);
      if (cfg_verbosity > 1)
         $display("    => MODULE_EXCEPTION_RSP");
    end
    else begin
      rg_rd_data <= tagged Valid zeroExtend(mem_rsp.rdata);
      // After reading fabric memory, redo the request
      rg_state <= MODULE_RUNNING;
    end
`ifdef NO_FABRIC_BOOTROM
  end
`endif
  endrule

  // ----------------------------------------------------------------
  // Provide write-response (ST op)
  // Stays in this state until CPU's next request puts it back into RUNNING state

  rule rl_ST_AMO_response(rg_state == MEM_ST_AMO_RSP);
    dw_valid <= True;
    // Irrelevant for ST; relevant for SC, AMO
    dw_output_ld_val <= zeroExtend(rg_ld_val);
    dw_output_st_amo_val <= zeroExtend(rg_st_amo_val);
  endrule

  // ----------------------------------------------------------------
  // Memory-mapped I/O read requests (LD and AMO_LR)
  // LRs are treated just like LDs, but we do not place any reservation on the
  // address (so a subsequent SC is guaranteed to fail).

  rule rl_io_read_req((rg_state == IO_REQ) && ((rg_op == CACHE_LD) || is_AMO_LR)
      && (ctr_wr_rsps_pending.value == 0));
    if (cfg_verbosity > 1)
      $display ("%0d: %s.rl_io_read_req; f3 0x%0h paddr %0h",
          cur_cycle, d_or_i, rg_f3, rg_addr);

`ifdef NO_FABRIC_PLIC
    if (soc_map.m_is_plic_addr(fn_PA_to_Fabric_Addr(rg_addr))) begin
      let nr_req = Near_Reg_Rd_Req { araddr: truncate(rg_addr) };
      f_plic_rd_req.enq(nr_req);
      if (cfg_verbosity > 1)
        $display ("%0d: %s.rl_io_read_req: PLIC RD: %0h", cur_cycle,
            d_or_i, rg_addr);
    end
    else begin
`endif
`ifdef NO_FABRIC_CLINT
      if (soc_map.m_is_near_mem_IO_addr(fn_PA_to_Fabric_Addr(rg_addr))) begin
        f_clint_rd_req.enq(Near_Reg_Rd_Req { araddr: zeroExtend(rg_addr) });
        if (cfg_verbosity > 1)
          $display ("%0d: %s.rl_io_read_req: CLINT RD: %0h", cur_cycle,
              d_or_i, rg_addr);
      end
      else begin
`endif
        Fabric_Addr fabric_addr = fn_PA_to_Fabric_Addr(rg_addr);
        fa_fabric_send_read_req(fabric_addr, fn_funct3_to_AXI4_Size(rg_f3));
`ifdef NO_FABRIC_CLINT
      end
`endif
`ifdef NO_FABRIC_PLIC
    end
`endif

`ifdef ISA_A
    // Invalidate LR/SC reservation if AMO_LR
    if (is_AMO_LR) rg_lrsc_valid <= False;
`endif
    rg_state <= IO_AWAITING_RD_RSP;
  endrule

  // ----------------------------------------------------------------
  // Receive I/O read response from fabric

  rule rl_io_read_rsp(rg_state == IO_AWAITING_RD_RSP &&
      (fn_is_fabric_req || fn_is_io_rd_resp_pending));
    Bit#(64) rdata;
    Bool resp_ok;
`ifdef NO_FABRIC_PLIC
    if (f_plic_rd_resp.notEmpty) begin
      let rd_data = f_plic_rd_resp.first;
      f_plic_rd_resp.deq;
      // TODO: this is a workaround. The correct solution is to parametrize
      // data widths throughout the design, instead of fixing it at 64 bits
      rdata = {rd_data.rdata, rd_data.rdata};
      resp_ok = rd_data.rresp == RESP_OK;
      if (cfg_verbosity > 2) begin
        $display("%0d: %s.rl_io_read_rsp: PLIC RD", cur_cycle, d_or_i);
        $display("        ", fshow(rd_data));
      end
    end
    else begin
`endif
`ifdef NO_FABRIC_CLINT
      if (f_clint_rd_resp.notEmpty) begin
        let rd_data = f_clint_rd_resp.first;
        f_clint_rd_resp.deq;
        rdata = rd_data.rdata;
        resp_ok = rd_data.rresp == RESP_OK;
        if (cfg_verbosity > 2) begin
          $display("%0d: %s.rl_io_read_rsp: CLINT RD", cur_cycle, d_or_i);
          $display("        ", fshow(rd_data));
        end
      end
      else begin
`endif
        let rd_data <- pop_o(master_xactor.o_rd_data);
        rdata = rd_data.rdata;
        resp_ok = rd_data.rresp == axi4_resp_okay;
        if (cfg_verbosity > 1) begin
          $display ("%0d: %s.rl_io_read_rsp: paddr 0x%0h", cur_cycle, d_or_i, rg_addr);
          $display ("    ", fshow(rd_data));
        end
`ifdef NO_FABRIC_CLINT
      end
`endif
`ifdef NO_FABRIC_PLIC
    end
`endif

    let ld_val = fn_extract_and_extend_bytes(rg_f3, rg_addr, zeroExtend(rdata));
    rg_ld_val <= ld_val;

    // Successful read
    if (resp_ok) begin
      fa_drive_IO_read_rsp(rg_f3, rg_addr, ld_val);
      rg_state <= IO_RD_RSP;
    end
    // Bus error
    else begin
      rg_state <= MODULE_EXCEPTION_RSP;
      rg_exc_code <= exc_code_LOAD_ACCESS_FAULT;
      if (cfg_verbosity > 1)
        $display ("%0d: %s.rl_io_read_rsp: FABRIC_RSP_ERR: raising trap LOAD_ACCESS_FAULT",
            cur_cycle, d_or_i);
    end
  endrule

  // ----------------
  // Maintain Mem-read response
  // Stays in this state until CPU's next request puts it back into RUNNING state

  rule rl_maintain_mem_read_rsp(rg_state == MEM_RD_RSP);
    fa_drive_mem_rsp(rg_f3, rg_addr, rg_rd_data.Valid, 0);
  endrule

  // ----------------
  // Maintain I/O-read response
  // Stays in this state until CPU's next request puts it back into RUNNING state

  rule rl_maintain_io_read_rsp(rg_state == IO_RD_RSP);
    fa_drive_IO_read_rsp(rg_f3, rg_addr, rg_ld_val);
  endrule

  // ----------------------------------------------------------------
  // Memory-mapped I/O write requests (ST)

  rule rl_io_write_req((rg_state == IO_REQ) && (rg_op == CACHE_ST));
    if (cfg_verbosity > 1)
      $display ("%0d: %s: rl_io_write_req; f3 0x%0h paddr %0h  word64 0x%0h",
          cur_cycle, d_or_i, rg_f3, rg_addr, rg_st_amo_val);

`ifdef NO_FABRIC_PLIC
    if (soc_map.m_is_plic_addr(fn_PA_to_Fabric_Addr(rg_addr)))
      f_plic_wr_req.enq(Near_Reg_Wr_Req{
        awaddr: rg_addr, 
        wdata: truncate(rg_st_amo_val),
        wstrb: 0
      });
    else
`endif
`ifdef NO_FABRIC_CLINT
      if (soc_map.m_is_near_mem_IO_addr(fn_PA_to_Fabric_Addr(rg_addr))) begin
        // TODO: use a function specific to near_rg_ifc here
        match {
            .waddr,
            .wdata,
            .wstrb,
            .wsize } = fn_to_fabric_write_fields(rg_f3, rg_addr, rg_st_amo_val);
        f_clint_wr_req.enq(Near_Reg_Wr_Req{
          awaddr: zeroExtend(waddr), 
          wdata: truncate(wdata),
          wstrb: wstrb
        });
      end else
`endif
        fa_fabric_send_write_req(rg_f3, rg_addr, rg_st_amo_val);

    rg_state <= MEM_ST_AMO_RSP;
    if (cfg_verbosity > 1)
      $display ("    => rl_ST_AMO_response");
  endrule

  // ----------------------------------------------------------------
  // Memory-mapped I/O AMO_SC requests. Always fail.

`ifdef ISA_A
  rule rl_io_AMO_SC_req((rg_state == IO_REQ) && is_AMO_SC);
    rg_ld_val <= 1;    // 1 is LR/SC failure value
    rg_state <= MEM_ST_AMO_RSP;

    if (cfg_verbosity > 1) begin
      $display ("%0d: %s: rl_io_AMO_SC_req; f3 0x%0h paddr %0h  word64 0x%0h",
          cur_cycle, d_or_i, rg_f3, rg_addr, rg_st_amo_val);
      $display ("    FAIL due to I/O address.");
      $display ("    => rl_ST_AMO_response");
    end
  endrule
`endif

  // ----------------------------------------------------------------
  // Memory-mapped I/O AMO requests other than LR/SC
  // TODO: ?? Fail with STORE/AMO Access fault exception

`ifdef ISA_A
  rule rl_io_AMO_op_req((rg_state == IO_REQ) && is_AMO && (! is_AMO_LR) && 
      (! is_AMO_SC));
    if (cfg_verbosity > 1)
      $display ("%0d: %s.rl_io_AMO_op_req; f3 0x%0h paddr %0h",
          cur_cycle, d_or_i, rg_f3, rg_addr);

`ifdef NO_FABRIC_PLIC
    if (soc_map.m_is_plic_addr(fn_PA_to_Fabric_Addr(rg_addr)))
      f_plic_rd_req.enq(Near_Reg_Rd_Req{
        araddr: rg_addr
      });
    else begin
`endif
`ifdef NO_FABRIC_CLINT
      if (soc_map.m_is_near_mem_IO_addr(fn_PA_to_Fabric_Addr(rg_addr)))
        f_clint_rd_req.enq(Near_Reg_Rd_Req{
          araddr: rg_addr
        });
      else begin
`endif
        Fabric_Addr fabric_addr = fn_PA_to_Fabric_Addr(rg_addr);
        fa_fabric_send_read_req(fabric_addr, fn_funct3_to_AXI4_Size(rg_f3));
`ifdef NO_FABRIC_CLINT
      end
``endif
`ifdef NO_FABRIC_PLIC
    end
``endif
    rg_state <= IO_AWAITING_AMO_RD_RSP;
  endrule
`endif

  // ----------------
  // Receive I/O AMO read response from fabric,
  // Do the AMO op, and send store to fabric

`ifdef ISA_A
  rule rl_io_AMO_read_rsp(rg_state == IO_AWAITING_AMO_RD_RSP &&
      (fn_is_fabric_req || fn_is_io_rd_resp_pending));
    Bit#(64) rdata;
    Bool resp_ok;
`ifdef NO_FABRIC_PLIC
    if (f_plic_rd_resp.notEmpty) begin
      let rd_data = f_plic_rd_resp.first;
      f_plic_rd_resp.deq;
      // TODO: this is a workaround. The correct solution is to parametrize
      // data widths throughout the design, instead of fixing it at 64 bits
      rdata = {rd_data.rdata, rd_data.rdata};
      resp_ok = rd_data.rresp == RESP_OK;
      if (cfg_verbosity > 2) begin
        $display("%0d: %s.rl_io_AMO_read_rsp: PLIC RD", cur_cycle, d_or_i);
        $display("        ", fshow(rd_data));
      end
    end
    else begin
`endif
`ifdef NO_FABRIC_CLINT
      if (f_clint_rd_resp.notEmpty) begin
        let rd_data = f_clint_rd_resp.first;
        f_clint_rd_resp.deq;
        rdata = rd_data.rdata;
        resp_ok = rd_data.rresp == RESP_OK;
        if (cfg_verbosity > 2) begin
          $display("%0d: %s.rl_io_AMO_read_rsp: CLINT RD", cur_cycle, d_or_i);
          $display("        ", fshow(rd_data));
        end
      end
      else begin
`endif
      let rd_data <- pop_o(master_xactor.o_rd_data);
      rdata = rd_data.rdata;
      resp_ok = rd_data.rresp == axi4_resp_okay;
      if (cfg_verbosity > 1) begin
        $display("%0d: %s.rl_io_AMO_read_rsp: paddr 0x%0h", cur_cycle, d_or_i,
            rg_addr);
        $display ("    ", fshow(rd_data));
      end
`ifdef NO_FABRIC_CLINT
      end
`endif      
`ifdef NO_FABRIC_PLIC
    end
`endif

    let ld_val = fn_extract_and_extend_bytes(rg_f3, rg_addr, zeroExtend(rdata));

    // Bus error for AMO read
    if (!resp_ok) begin
      rg_state <= MODULE_EXCEPTION_RSP;
      rg_exc_code <= exc_code_STORE_AMO_ACCESS_FAULT;
      if (cfg_verbosity > 1)
        $display ("%0d: %s.rl_io_AMO_read_rsp: FABRIC_RSP_ERR: raising trap STORE_AMO_ACCESS_FAULT",
            cur_cycle, d_or_i);
    end
    // Successful AMO read
    else begin
      if (cfg_verbosity > 1)
        $display ("%0d: %s: rl_io_AMO_read_rsp; f3 0x%0h  paddr %0h  word64 0x%0h",
            cur_cycle, d_or_i, rg_f3, rg_addr, rg_st_amo_val);

      // Do the AMO op on the loaded value and the store value
      match {.new_ld_val, .new_st_val} = fn_amo_op(rg_f3, rg_amo_funct7,
          rg_addr, ld_val, rg_st_amo_val);

`ifdef NO_FABRIC_PLIC
      if (soc_map.m_is_plic_addr(fn_PA_to_Fabric_Addr(rg_addr)))
        f_plic_wr_req.enq(Near_Reg_Wr_Req{
          awaddr: rg_addr, 
          wdata: truncate(new_st_val),
          wstrb: 0
        });
      else
`endif
`ifdef NO_FABRIC_CLINT
        if (soc_map.m_is_near_mem_IO_addr(fn_PA_to_Fabric_Addr(rg_addr))) begin
          // TODO: use a function specific to near_rg_ifc here
          match {
              .waddr,
              .wdata,
              .wstrb,
              .wsize } = fn_to_fabric_write_fields(rg_f3, rg_addr, new_st_val);
          f_clint_wr_req.enq(Near_Reg_Wr_Req{
            awaddr: zeroExtend(waddr), 
            wdata: truncate(wdata),
            wstrb: wstrb
          });
        end else
`endif
          // Write back new st_val to fabric
          fa_fabric_send_write_req(rg_f3, rg_addr, new_st_val);

      fa_drive_IO_read_rsp(rg_f3, rg_addr, new_ld_val);
      rg_ld_val <= new_ld_val;
      rg_state <= IO_RD_RSP;

      if (cfg_verbosity > 1)
        $display ("    => rl_ST_AMO_response");
    end
  endrule
`endif

  // ----------------------------------------------------------------
  // Discard write-responses from the fabric
  // NOTE: assuming in-order responses from fabric

  rule rl_discard_write_rsp;
    let wr_resp <- pop_o(master_xactor.o_wr_resp);

    if (ctr_wr_rsps_pending.value == 0) begin
      $display ("%0d: ERROR: %s.rl_discard_write_rsp: unexpected W response (ctr_wr_rsps_pending.value == 0)",
          cur_cycle, d_or_i);
      $display ("    ", fshow (wr_resp));
      $finish (1);    // Assertion failure
    end

    ctr_wr_rsps_pending.decr;

    if (wr_resp.bresp != axi4_resp_okay) begin
      // TODO: need to raise a non-maskable interrupt (NMI) here
      $display ("%0d: %s.rl_discard_write_rsp: fabric response error: exit",
          cur_cycle, d_or_i);
      $display ("    ", fshow(wr_resp));
    end
    else if (cfg_verbosity > 1) begin
      $display ("%0d: %s.rl_discard_write_rsp: pending %0d ",
          cur_cycle, d_or_i, ctr_wr_rsps_pending.value, fshow(wr_resp));
    end
  endrule

`ifdef NO_FABRIC_PLIC
  // Discard PLIC write responses
  rule discard_plic_wr_resp(f_plic_wr_resp.notEmpty);
    let wresp = f_plic_wr_resp.first;
    f_plic_wr_resp.deq;

    // TODO: do we need credit counters for PLIC, like for the fabric?

    if (wresp.bresp != RESP_OK) begin
      // TODO: need to raise a non-maskable interrupt (NMI) here
      $display("%0d: %s.discard_plic_wr_resp: PLIC response error: exit",
          cur_cycle, d_or_i);
      $display("    ", fshow(wresp));
    end
  endrule
`endif

`ifdef NO_FABRIC_CLINT
  // Discard CLINT write responses
  rule discard_clint_wr_resp(f_clint_wr_resp.notEmpty);
    let wresp = f_clint_wr_resp.first;
    f_clint_wr_resp.deq;

    // TODO: do we need credit counters for CLINT, like for the fabric?

    if (wresp.bresp != RESP_OK) begin
      // TODO: need to raise a non-maskable interrupt (NMI) here
      $display("%0d: %s.discard_plic_wr_resp: CLINT response error: exit",
          cur_cycle, d_or_i);
      $display("    ", fshow(wresp));
    end
  endrule
`endif

  // ----------------------------------------------------------------
  // This rule drives an exception response until this module is put
  // into MODULE_RUNNING state by the next request.

  rule rl_drive_exception_rsp(rg_state == MODULE_EXCEPTION_RSP);
    dw_valid <= True;
    dw_exc <= True;
    dw_exc_code <= rg_exc_code;
  endrule

  // ----------------------------------------------------------------
  // INTERFACE

  method Action set_verbosity(Bit#(4) v);
    cfg_verbosity <= v;
  endmethod

  // Reset
  interface Server server_reset;
    interface Put request;
      method Action put(Token t);
        f_reset_reqs.enq (?);
      endmethod
    endinterface

    interface Get response;
      method ActionValue#(Token) get();
        f_reset_rsps.deq;
        return ?;
      endmethod
    endinterface
  endinterface

  // CPU interface: request
  // NOTE: this has no flow control: CPU should only invoke it when consuming
  // prev output. As soon as this method is called, the module starts working 
  // on this new request.
  method Action req(
      CacheOp op,
      Bit #(3) f3,
`ifdef ISA_A
      Bit #(7) amo_funct7,
`endif
      Addr addr,
      Bit #(64) st_value);
    if (cfg_verbosity > 1) begin
      $display("%0d: %m.req: op:", cur_cycle, fshow (op),
          " f3:%0d addr:0x%0h st_value:0x%0h", f3, addr, st_value);
`ifdef ISA_A
      $display("    amo_funct7 = 0x%0h", amo_funct7);
`endif
    end

    rg_op <= op;
    rg_f3 <= f3;
`ifdef ISA_A
    rg_amo_funct7 <= amo_funct7;
`endif
    rg_addr <= addr;
    rg_st_amo_val <= st_value;
    rg_rd_data <= tagged Invalid;

    if (!fn_is_aligned(f3, addr)) begin
      // We detect misaligned accesses and trap on them
      if (cfg_verbosity > 5)
        $display("%0d: %m.req: => MODULE_EXCEPTION_RSP", cur_cycle);
      rg_state <= MODULE_EXCEPTION_RSP;
      rg_exc_code <= ((op == CACHE_LD) ? exc_code_LOAD_ADDR_MISALIGNED : 
          exc_code_STORE_AMO_ADDR_MISALIGNED);
    end
    else begin
      if (cfg_verbosity > 5)
        $display("%0d: %m.req: => MODULE_RUNNING", cur_cycle);
      rg_state <= MODULE_RUNNING;
    end
  endmethod

  method Bool valid;
    return dw_valid;
  endmethod

  method WordXL addr;    // req addr for which this is a response
    return rg_addr;
  endmethod

  method Bit#(64) word64;
    return dw_output_ld_val;
  endmethod

  method Bit#(64) st_amo_val;
    return dw_output_st_amo_val;
  endmethod

  method Bool exc;
    return dw_exc;
  endmethod

  method Exc_Code exc_code;
    return dw_exc_code;
  endmethod

  // Fabric master interface
  interface mem_master = master_xactor.axi_side;

`ifdef NO_FABRIC_BOOTROM
  // Boot ROM master interface
  interface boot_rom = toGPClient(f_boot_rom_req, f_boot_rom_resp);
`endif

`ifdef NO_FABRIC_PLIC
  // PLIC master interface
  interface Near_Reg_Master_IFC plic_reg_access;
    interface reg_rd = toGPClient(f_plic_rd_req, f_plic_rd_resp);
    interface reg_wr = toGPClient(f_plic_wr_req, f_plic_wr_resp);
  endinterface
`endif

`ifdef NO_FABRIC_CLINT
  // CLINT master interface
  interface Near_Reg_Master_IFC clint_reg_access;
    interface reg_rd = toGPClient(f_clint_rd_req, f_clint_rd_resp);
    interface reg_wr = toGPClient(f_clint_wr_req, f_clint_wr_resp);
  endinterface
`endif
endmodule

// ----------------------------------------------------------------
// DMEM and IMEM specializations

module mkDMEM_PT(Passthru_IFC);
  Passthru_IFC dmem <- mkPassthru(True);
  return dmem;
endmodule

module mkIMEM_PT(Passthru_IFC);
  Passthru_IFC imem <- mkPassthru(False);
  return imem;
endmodule

endpackage