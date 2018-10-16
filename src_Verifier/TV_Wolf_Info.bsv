/*



*/

package TV_Wolf_Info;

import DefaultValue :: *;
import ISA_Decls :: *;

// Define instruction and register sizes

// Clifford Wolf RISC-V Formal Interface
// (http://www.clifford.at/papers/2017/riscv-formal/slides.pdf)
typedef struct {
    
    //Bit#(64)        rvfi_valid;     // Valid signal:                        Instruction was committed properly.
    Bit#(64)        rvfi_order;     // [000 - 063] Instruction number:      INSTRET value after completion.
    Bool            rvfi_trap;      // [064 - 064] Trap indicator:          Invalid decode, misaligned access or
                                    //                                      jump command to misaligned address.
    
    Bool            rvfi_halt;      // [065 - 065] Halt indicator:          Marks the last instruction retired 
                                    //                                      before halting execution.
                                    
    Bool            rvfi_intr;      // [066 - 066] Trap handler:            Set for first instruction in trap handler.
    
    Bit#(32)        rvfi_insn;      // [067 - 098] Instruction word:        32-bit command value.
    Bit#(5)         rvfi_rs1_addr;  // [099 - 103] Read register addresses: Can be arbitrary when not used,
    Bit#(5)         rvfi_rs2_addr;  // [104 - 108]                          otherwise set as decoded.
    Bit#(xlen)      rvfi_rs1_data;  // [109 - 172] Read register values:    Values as read from registers named
    Bit#(xlen)      rvfi_rs2_data;  // [173 - 236]                          above. Must be 0 if register ID is 0.
    Bit#(xlen)      rvfi_pc_rdata;  // [237 - 300] PC before instr:         PC for current instruction
    Bit#(xlen)      rvfi_pc_wdata;  // [301 - 364] PC after instr:          Following PC - either PC + 4 or jump target.
    
    // PROBLEM: LR/SC, if SC fails then value is not written. Indicate as wmask = 0.
    Bit#(xlen)      rvfi_mem_wdata; // [365 - 428] Write data:              Data written to memory by this command.

    // Found in ALU if used, not 0'd if not used. Check opcode/op_stage2.
    // PROBLEM: LD/AMO - then found in stage 2.
    Bit#(5)         rvfi_rd_addr;   // [429 - 433] Write register address:  MUST be 0 if not used.
    Bit#(xlen)      rvfi_rd_wdata;  // [434 - 497] Write register value:    MUST be 0 if rd_ is 0.

    // Found in ALU, conflicts with jump/branch target or CSR for CSRRX
    Bit#(xlen)      rvfi_mem_addr;  // [498 - 561] Memory access addr:      Points to byte address (aligned if define
                                    //                                      is set). *Should* be straightforward.

    // Not explicitly given, but calculable from opcode/funct3 from ISA_Decls.
    Bit#(masklen)   rvfi_mem_rmask; // [562 - 569] Read mask:               Indicates valid bytes read. 0 if unused.
    Bit#(masklen)   rvfi_mem_wmask; // [570 - 577] Write mask:              Indicates valid bytes written. 0 if unused.
    
    // XXX: SC writes something other than read value, but the value that would be read is unimportant.
    // Unsure what the point of this is, it's only relevant when the value is going to be in rd anyway.
    Bit#(xlen)      rvfi_mem_rdata; // [578 - 641] Read data:               Data read from mem_addr (i.e. before write)
    
    

} Riscv_Formal#(numeric type xlen, numeric type masklen) deriving (Bits, Eq);

typedef Riscv_Formal#(XLEN,MASKLEN) Info_CPU_to_Verifier;


function String getString(Bool x);
    if (x)
        return " True";
    else
        return "False";
endfunction


endpackage
