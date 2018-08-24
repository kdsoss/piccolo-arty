/*



*/

package Verifier_CPU;

//export mkVerifier_CPU, Verif_IFC(..);

import Memory           :: *;
import GetPut           :: *;
import ClientServer     :: *;

import Verifier         :: *;
import TV_Wolf_Info     :: *;
import ISA_Decls        :: *;

import CPU_IFC          :: *;
import Verif_IFC        :: *;
import Fabric_Defs      :: *;
import AXI4_Lite_Types  :: *;
import CPU_Globals      :: *;
import FIFOF            :: *;
import CPU              :: *;

module mkVerifier_CPU #(parameter Bit#(64) pc_reset_value) (Verif_IFC);
    
    // Allow some buffering in case of hidden delays
    //FIFOF#(Info_CPU_to_Verifier) packet <- mkFIFOF;
    
    // The core we are going to verify
    CPU_IFC core <- mkCPU(pc_reset_value);

//`ifdef SIM
    /*rule displayData;
        Info_CPU_to_Verifier x <- core.to_verifier.get;
        $display("[[ 0x%4h ]], trap: %s, halt: %s, insn:0x%8h, pc:0x%8h, rs1: 0x%5b, rs2: 0x%5b, rd: 0x%5b",
                    x.rvfi_order,getString(x.rvfi_trap),getString(x.rvfi_halt), x.rvfi_insn,
                    x.rvfi_pc_rdata, x.rvfi_rs1_addr, x.rvfi_rs2_addr, x.rvfi_rd_addr);
         //$display("[[%4d]] pc: 0x%0h, insn: 0x%0h, wmask: 0x%0h, rmask: 0x%0h,
         //           mem_rdata: 0x%0h, rs1_data: 0x%0h, rs2_data: 0x%0h", x.rvfi_order,  
         //           x.rvfi_pc_rdata, x.rvfi_insn, x.rvfi_mem_wmask, x.rvfi_mem_rmask,   
         //           x.rvfi_mem_rdata, x.rvfi_rs1_data, x.rvfi_rs2_data);             
    endrule*/
//`elsif VERILOG
    method ActionValue#(Info_CPU_to_Verifier) getPacket() = core.to_verifier.get;
    method Bool halted = core.halted;
//`endif
    method Action external_interrupt_req  = core.external_interrupt_req;
    method Action timer_interrupt_req (b) = core.timer_interrupt_req(b);
    method Action software_interrupt_req  = core.software_interrupt_req;
    
    interface Server hart0_server_reset = core.hart0_server_reset;
    
    interface imem_master    =  core.imem_master;
    interface dmem_master    =  core.dmem_master;
    interface near_mem_slave =  core.near_mem_slave;
    
`ifdef INCLUDE_GDB_CONTROL
    
    interface Server hart0_server_run_halt = core.hart0_server_run_halt;
    interface Put    hart0_put_other_req   = core.hart0_put_other_req;
    
    interface MemoryServer hart0_gpr_mem_server = core.hart0_gpr_mem_server;
    interface MemoryServer hart0_csr_mem_server = core.hart0_csr_mem_server;
   
`endif
    
endmodule : mkVerifier_CPU

endpackage
