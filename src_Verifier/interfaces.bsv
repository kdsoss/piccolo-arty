/*



    *Header stuff*



*/

// Define instruction and register sizes
typedef 32 ILEN;
`ifdef RV64
typedef 64 XLEN;
typedef 8  MASKLEN;
`else
typedef 32 XLEN;
typedef 4  MASKLEN;
`endif

// Clifford Wolf Formal Interface
// (http://www.clifford.at/papers/2017/riscv-formal/slides.pdf)
typedef struct {
    Bool            rvfi_valid;
    Bit#(64)        rvfi_order;
    Bit#(ILEN)      rvfi_insn;
    Bool            rvfi_trap;
    Bool            rvfi_halt;
    Bool            rvfi_intr;
    Bit#(5)         rvfi_rs1_addr;
    Bit#(XLEN)      rvfi_rs1_data;
    Bit#(5)         rvfi_rs2_addr;
    Bit#(XLEN)      rvfi_rs2_data;
    Bit#(5)         rvfi_rd_addr;
    Bit#(XLEN)      rvfi_rd_wdata;
    Bit#(XLEN)      rvfi_pc_rdata;
    Bit#(XLEN)      rvfi_pc_wdata;
    Bit#(XLEN)      rvfi_mem_addr;
    Bit#(MASKLEN)   rvfi_mem_rmask;
    Bit#(MASKLEN)   rvfi_mem_wmask;
    Bit#(XLEN)      rvfi_mem_rdata;
    Bit#(XLEN)      rvfi_mem_wdata;
} riscv_formal deriving (Bits, Eq);

// Interface for getting verification data from implementation/model
interface Verif_IFC;

    // Can either return the simple struct and handle the details elsewhere...
    method riscv_formal getVerifs;
    
    // ... or offer the values from individual methods.
    /*
    method Bool             get_valid;
    method Bit#(64)         get_order;
    method Bit#(ILEN)       get_insn;
    method Bool             get_trap;
    method Bool             get_halt;
    method Bool             get_intr;
    method Bit#(5)          get_rs1_addr;
    method Bit#(XLEN)       get_rs1_data;
    method Bit#(5)          get_rs2_addr;
    method Bit#(XLEN)       get_rs2_data;
    method Bit#(5)          get_rd_addr;
    method Bit#(XLEN)       get_rd_wdata;
    method Bit#(XLEN)       get_pc_rdata;
    method Bit#(XLEN)       get_pc_wdata;
    method Bit#(XLEN)       get_mem_addr;
    method Bit#(XLEN)       get_mem_rdata;
    method Bit#(XLEN)       get_mem_wdata;
    method Bit#(MASKLEN)    get_mem_rmask;
    method Bit#(MASKLEN)    get_mem_wmask;
    */

endinterface
