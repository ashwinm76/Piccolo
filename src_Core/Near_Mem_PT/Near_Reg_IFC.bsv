package Near_Reg_IFC;


// ================================================================
// Interface to access "near" peripherals, which are connected to
// the CPU directly, i.e. not via a fabric

// ================================================================
// Bluespec lib imports

import  ClientServer :: *;
import  Connectable  :: *;

// ================================================================
// Types

typedef enum { 
  RESP_OK, RESP_DECERR, RESP_ERR 
} Near_Resp_Status deriving(Eq, Bits, FShow);

typedef struct {
  Bit#(32) araddr;
} Near_Reg_Rd_Req deriving(Bits, FShow);

typedef struct {
  Bit#(32) awaddr;
  Bit#(32) wdata;
} Near_Reg_Wr_Req deriving(Bits, FShow);

typedef struct {
  Near_Resp_Status rresp;
  Bit#(32) rdata;
} Near_Reg_Rd_Resp deriving(Bits, FShow);

typedef struct {
  Near_Resp_Status bresp;
} Near_Reg_Wr_Resp deriving(Bits, FShow);

// ================================================================
// Interfaces

interface Near_Reg_Slave_IFC;
  interface Server#(Near_Reg_Rd_Req, Near_Reg_Rd_Resp) reg_rd;
  interface Server#(Near_Reg_Wr_Req, Near_Reg_Wr_Resp) reg_wr;
endinterface

interface Near_Reg_Master_IFC;
  interface Client#(Near_Reg_Rd_Req, Near_Reg_Rd_Resp) reg_rd;
  interface Client#(Near_Reg_Wr_Req, Near_Reg_Wr_Resp) reg_wr;
endinterface

instance Connectable#(Near_Reg_Master_IFC, Near_Reg_Slave_IFC);
  module mkConnection#(Near_Reg_Master_IFC m, Near_Reg_Slave_IFC s)(Empty);
    mkConnection(m.reg_rd, s.reg_rd);
    mkConnection(m.reg_wr, s.reg_wr);
  endmodule
endinstance

endpackage