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
  Bit#(wd_addr) araddr;
} Near_Reg_Rd_Req#(numeric type wd_addr) deriving(Bits, FShow);

typedef struct {
  Bit#(wd_addr) awaddr;
  Bit#(wd_data) wdata;
  Bit#(TDiv#(wd_data, 8)) wstrb;
} Near_Reg_Wr_Req#(numeric type wd_addr, numeric type wd_data) 
    deriving(Bits, FShow);

typedef struct {
  Near_Resp_Status rresp;
  Bit#(wd_data) rdata;
} Near_Reg_Rd_Resp#(numeric type wd_data) deriving(Bits, FShow);

typedef struct {
  Near_Resp_Status bresp;
} Near_Reg_Wr_Resp deriving(Bits, FShow);

// ================================================================
// Interfaces

interface Near_Reg_Slave_IFC#(numeric type wd_addr, numeric type wd_data);
  interface Server#(Near_Reg_Rd_Req#(wd_addr), Near_Reg_Rd_Resp#(wd_data)) reg_rd;
  interface Server#(Near_Reg_Wr_Req#(wd_addr, wd_data), Near_Reg_Wr_Resp) reg_wr;
endinterface

interface Near_Reg_Master_IFC#(numeric type wd_addr, numeric type wd_data);
  interface Client#(Near_Reg_Rd_Req#(wd_addr), Near_Reg_Rd_Resp#(wd_data)) reg_rd;
  interface Client#(Near_Reg_Wr_Req#(wd_addr, wd_data), Near_Reg_Wr_Resp) reg_wr;
endinterface

instance Connectable#(
    Near_Reg_Master_IFC#(wd_addr, wd_data), 
    Near_Reg_Slave_IFC#(wd_addr, wd_data));
  module mkConnection#(
      Near_Reg_Master_IFC#(wd_addr, wd_data) m, 
      Near_Reg_Slave_IFC#(wd_addr, wd_data) s)(Empty);
    mkConnection(m.reg_rd, s.reg_rd);
    mkConnection(m.reg_wr, s.reg_wr);
  endmodule
endinstance

endpackage