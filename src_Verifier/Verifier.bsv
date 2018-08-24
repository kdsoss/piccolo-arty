/*



*/

package Verifier;


`ifdef INCLUDE_TANDEM_VERIF
import TV_Info :: *;
`elsif INCLUDE_WOLF_VERIF
import TV_Wolf_Info :: *;
`endif
import ISA_Decls   :: *;
import CPU_Globals :: *;

`ifdef INCLUDE_WOLF_VERIF
// This function relies on info that is only passed in Wolf-mode.

Bit#(32) ecall_insn = 32'h73;

function Info_CPU_to_Verifier getWolfInfoCondensed(
            // Using the full data gives us access to rd_valid and a few other fields.
            Data_Stage2_to_Stage3   data_s2_s3,
            Bit#(64)                order,
            Bool                    isTrap,
            Exc_Code                exc,
            Bool                    handler,
            Bool                    halted
            );
            
    Data_Wolf_Stage2 s2 = data_s2_s3.wolf_info_s2;
    Data_Wolf_Stage1 s1 = s2.stage1;
    
    return Info_CPU_to_Verifier {
        rvfi_order:     order,
        // Not all traps are relevant in the Clifford-Wolf framework, e.g. page faults.
        rvfi_trap:      isTrap,
        rvfi_halt:      (halted) ? True : (data_s2_s3.instr == ecall_insn),
        rvfi_intr:      handler,
        rvfi_insn:      data_s2_s3.instr,
        rvfi_rs1_addr:  s1.rs1_addr,
        rvfi_rs2_addr:  s1.rs2_addr,
        rvfi_rd_addr:   (data_s2_s3.rd_valid ? data_s2_s3.rd : 0),
        rvfi_rs1_data:  s1.rs1_data,
        rvfi_rs2_data:  s1.rs2_data,
        rvfi_rd_wdata:  (data_s2_s3.rd == 0 ? 0 : data_s2_s3.rd_val),
        rvfi_pc_rdata:  data_s2_s3.pc,
        // TODO: Check this with regard to exceptions and the like.
        rvfi_pc_wdata:  s1.pc_wdata,
        rvfi_mem_wdata: s1.mem_wdata,
        rvfi_mem_addr:  s1.mem_addr,
        rvfi_mem_rmask: s2.mem_rmask,
        rvfi_mem_wmask: s2.mem_wmask,
        rvfi_mem_rdata: data_s2_s3.rd_val
    };
endfunction : getWolfInfoCondensed
            
            
            
function Info_CPU_to_Verifier getWolfInfoS1 (
            Data_Stage1_to_Stage2   data_s1_s2,
            Bit#(64)                order,
            Bool                    isTrap,
            Exc_Code                exc,
            Bool                    handler,
            Bool                    halted
            );
    
    Data_Wolf_Stage1 s1 = data_s1_s2.wolf_info_s1;
    
    return Info_CPU_to_Verifier {
        rvfi_order:     order,
        rvfi_trap:      ( isTrap ? (exc[0] == 1'b0 && exc < 7) : (False) ),
        rvfi_halt:      (halted) ? True : (data_s1_s2.instr == ecall_insn),
        rvfi_intr:      handler,
        rvfi_insn:      data_s1_s2.instr,
        rvfi_rs1_addr:  s1.rs1_addr,
        rvfi_rs2_addr:  s1.rs2_addr,
        rvfi_rs1_data:  s1.rs1_data,
        rvfi_rs2_data:  s1.rs2_data,
        rvfi_pc_rdata:  data_s1_s2.pc,
        rvfi_pc_wdata:  s1.pc_wdata,
        rvfi_mem_addr:  s1.mem_addr,
        // Although we know what rd *would* be, the fact that we're using this function
        // means we can't have actually written to it.
        rvfi_rd_addr:   0,
        rvfi_rd_wdata:  0,
        rvfi_mem_rmask: 0,
        rvfi_mem_wmask: 0,
        rvfi_mem_wdata: 0,
        rvfi_mem_rdata: 0
    };
endfunction : getWolfInfoS1

`endif


// Think this is correct?
function Bit #(8) getMemMask(Bit #(3) f3, Bit #(XLEN) addr);
    Bit #(8) result    = 0;
    Bit #(3)  addr_lsbs = addr [2:0];

    case (f3)
        f3_LB: case (addr_lsbs)
		    'h0: result = 8'b0000_0001;
		    'h1: result = 8'b0000_0010;
		    'h2: result = 8'b0000_0100;
		    'h3: result = 8'b0000_1000;
		    'h4: result = 8'b0001_0000;
		    'h5: result = 8'b0010_0000;
		    'h6: result = 8'b0100_0000;
		    'h7: result = 8'b1000_0000;
	        endcase
        f3_LBU: case (addr_lsbs)
		    'h0: result = 8'b0000_0001;
		    'h1: result = 8'b0000_0010;
		    'h2: result = 8'b0000_0100;
		    'h3: result = 8'b0000_1000;
		    'h4: result = 8'b0001_0000;
		    'h5: result = 8'b0010_0000;
		    'h6: result = 8'b0100_0000;
		    'h7: result = 8'b1000_0000;
	        endcase

        f3_LH: case (addr_lsbs)
		    'h0: result = 8'b0000_0011;
		    'h2: result = 8'b0000_1100;
		    'h4: result = 8'b0011_0000;
		    'h6: result = 8'b1100_0000;
	        endcase
        f3_LHU: case (addr_lsbs)
		    'h0: result = 8'b0000_0011;
		    'h2: result = 8'b0000_1100;
		    'h4: result = 8'b0011_0000;
		    'h6: result = 8'b1100_0000;
	        endcase

        f3_LW: case (addr_lsbs)
		    'h0: result = 8'b0000_1111;
		    'h4: result = 8'b1111_0000;
	        endcase
	     
	     // LWU and LD only appear in RV64I.
	    `ifdef RV64
        f3_LWU: case (addr_lsbs)
		    'h0: result = 8'b0000_1111;
		    'h4: result = 8'b1111_0000;
	        endcase

        f3_LD: case (addr_lsbs)
		    'h0: result = 8'b1111_1111;
	        endcase
	    `endif
    endcase
    return result;
endfunction : getMemMask



endpackage

