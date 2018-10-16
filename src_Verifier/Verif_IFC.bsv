package Verif_IFC;

import Memory           :: *;
import GetPut           :: *;
import ClientServer     :: *;

import Verifier         :: *;
import TV_Wolf_Info     :: *;
import ISA_Decls        :: *;

import Fabric_Defs      :: *;
import AXI4_Lite_Types  :: *;
import CPU_Globals      :: *;

interface Verif_IFC;
    // Standard CPU interfaces that we will pass through

    interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User) imem_master;
    interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User) dmem_master;
    interface AXI4_Lite_Slave_IFC  #(Wd_Addr, Wd_Data, Wd_User) near_mem_slave;
    
    interface Server #(Token, Token)  hart0_server_reset;
    
    method Action external_interrupt_req (Bool set_not_clear);
    method Action timer_interrupt_req (Bool set_not_clear);
    method Action software_interrupt_req (Bool set_not_clear);
    
`ifdef INCLUDE_GDB_CONTROL
    
    interface Server #(Bool, Bool) hart0_server_run_halt;
    interface Put #(Bit #(4)) hart0_put_other_req;
    
    // GPR access
    interface MemoryServer #(5,  XLEN)  hart0_gpr_mem_server;

    // CSR access
    interface MemoryServer #(12, XLEN)  hart0_csr_mem_server;
    
`endif

`ifdef VERILOG
    (* ready = "available", enable = "sent" *)
    method ActionValue#(Info_CPU_to_Verifier) getPacket;
    (*always_ready, always_enabled *)
    method Bool halted;
`elsif TANDEM
    (* ready = "available", enable = "sent" *)
    method ActionValue#(Info_CPU_to_Verifier) getPacket;
    (*always_ready, always_enabled *)
    method Bool halted;
`endif

endinterface

endpackage
