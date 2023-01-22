// This is a pass-through implementation of the Near Mem interface. Requests
// from the CPU are simply passed through to the fabric.

package Near_Mem_PT;

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

// ================================================================
// Project imports

import ISA_Decls    :: *;
import Near_Mem_IFC :: *;
import AXI4_Types   :: *;
import Fabric_Defs  :: *;
import Passthru     :: *;

`ifdef NO_FABRIC_BOOTROM
import Boot_ROM     :: *;
`endif

`ifdef NO_FABRIC_PLIC
import Near_Reg_IFC :: *;
`endif

`ifdef NO_FABRIC_CLINT
import Near_Mem_IO_AXI4 :: *;
`endif

// System address map and pc_reset value
import SoC_Map :: *;

// ================================================================
// Exports

export mkNear_Mem;

// ================================================================
// The module

// Module state
typedef enum {STATE_RESET, STATE_RESETTING, STATE_READY} State
deriving (Bits, Eq, FShow);

(* synthesize *)
module mkNear_Mem(Near_Mem_IFC);

  Reg#(Bit#(4)) cfg_verbosity <- mkConfigReg(0);
  Reg#(State) rg_state <- mkReg(STATE_READY);

  // Reset response queue
  FIFOF#(Token) f_reset_rsps <- mkFIFOF;

  Passthru_IFC imem_pt <- mkIMEM_PT;
  Passthru_IFC dmem_pt <- mkDMEM_PT;

`ifdef NO_FABRIC_BOOTROM
  Boot_ROM_IFC boot_rom <- mkBoot_ROM;
  mkConnection(imem_pt.boot_rom, boot_rom.imem);
  mkConnection(dmem_pt.boot_rom, boot_rom.dmem);
`endif

  // ----------------------------------------------------------------
  // BEHAVIOR

  // ----------------
  // Reset
  // This reset state machine operates on external soft-reset request.

  rule rl_reset(rg_state == STATE_RESET);
    rg_state <= STATE_RESETTING;
    imem_pt.server_reset.request.put(?);
    dmem_pt.server_reset.request.put(?);
    if (cfg_verbosity > 1)
	    $display ("%0d: Near_Mem.rl_reset", cur_cycle);
  endrule

  rule rl_reset_complete(rg_state == STATE_RESETTING);
    let _dummy1 <- imem_pt.server_reset.response.get;
    let _dummy2 <- dmem_pt.server_reset.response.get;
    f_reset_rsps.enq(?);
    rg_state <= STATE_READY;
    if (cfg_verbosity > 1)
	    $display ("%0d: Near_Mem.rl_reset_complete", cur_cycle);
  endrule

  // ----------------------------------------------------------------
  // INTERFACE

  // Reset
  interface Server server_reset;
    interface Put request;
	    method Action put(Token t) if (rg_state == STATE_READY);
	      rg_state <= STATE_RESET;
	    endmethod
    endinterface

    interface Get response;
	    method ActionValue#(Token) get();
	      let rsp <- pop(f_reset_rsps);
	      return rsp;
	    endmethod
    endinterface
  endinterface

  // ----------------
  // IMem

  // CPU side
  interface IMem_IFC imem;
    // CPU side: IMem request
    method Action req(Bit#(3) f3,
		    WordXL addr,
			  // The following  args for VM
			  Priv_Mode  priv,
			  Bit #(1)   sstatus_SUM,
			  Bit #(1)   mstatus_MXR,
			  WordXL     satp);    // { VM_Mode, ASID, PPN_for_page_table }
      Bit #(7)  amo_funct7  = ?;
	    Bit #(64) store_value = ?;
	    imem_pt.req(CACHE_LD, f3,
`ifdef ISA_A
		      amo_funct7,
`endif
		      addr, store_value);
    endmethod

    // CPU side: IMem response
    method Bool valid = imem_pt.valid;
    method Bool is_i32_not_i16 = True;
    method WordXL pc = imem_pt.addr;
    method Instr instr = truncate(imem_pt.word64);
    method Bool exc = imem_pt.exc;
    method Exc_Code exc_code = imem_pt.exc_code;
    method WordXL tval = imem_pt.addr;
   endinterface

   // Fabric side
   interface imem_master = imem_pt.mem_master;

  // ----------------
  // DMem

  // CPU side
  interface DMem_IFC dmem;
    // CPU side: DMem request
    method Action req(CacheOp op,
		    Bit #(3) f3,
`ifdef ISA_A
			  Bit #(7) amo_funct7,
`endif
			  WordXL addr,
			  Bit #(64) store_value,
			  // The following  args for VM
			  Priv_Mode  priv,
			  Bit #(1)   sstatus_SUM,
			  Bit #(1)   mstatus_MXR,
			  WordXL     satp);    // { VM_Mode, ASID, PPN_for_page_table }
      dmem_pt.req(op, f3,
`ifdef ISA_A
		      amo_funct7,
`endif
		      addr, store_value);
    endmethod

    // CPU side: DMem response
    method Bool valid = dmem_pt.valid;
    method Bit#(64) word64 = dmem_pt.word64;
`ifdef ISA_A
    method Bit#(64) st_amo_val = dmem_pt.st_amo_val;
`endif
    method Bool exc = dmem_pt.exc;
    method Exc_Code exc_code = dmem_pt.exc_code;
  endinterface

  // Fabric side
  interface dmem_master = dmem_pt.mem_master;

  // ----------------
  // FENCE.I: flush both ICache and DCache

  interface Server server_fence_i;
    interface Put request;
	    method Action put(Token t);
        noAction;
	    endmethod
    endinterface
    
    interface Get response;
	    method ActionValue#(Token) get;
	      return ?;
	    endmethod
    endinterface
  endinterface

  // ----------------
  // FENCE: flush DCache

  interface Server server_fence;
    interface Put request;
	    method Action put(Fence_Ordering t);
        noAction;
	    endmethod
    endinterface
    
    interface Get response;
	    method ActionValue#(Token) get;
	      return ?;
	    endmethod
    endinterface
  endinterface

  // ----------------
  // SFENCE_VMA: flush TLBs

  method Action sfence_vma;
    noAction;
  endmethod

  // ---------------
  // PLIC
`ifdef NO_FABRIC_PLIC
  interface plic = dmem_pt.plic_reg_access;
`endif

  // ---------------
  // CLINT
`ifdef NO_FABRIC_CLINT
  interface clint = dmem_pt.clint_reg_access;
`endif

endmodule

endpackage