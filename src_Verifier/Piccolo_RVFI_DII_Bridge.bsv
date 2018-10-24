/*-
 * Copyright (c) 2018 Peter Rugg
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

package Piccolo_RVFI_DII_Bridge;

// ================================================================
// BSV library imports

import FIFOF        :: *;
import FIFO         :: *;
import SpecialFIFOs :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import ConfigReg    :: *;

// ================================================================
// Project imports

import Near_Mem_IFC :: *; 
import ISA_Decls :: *;

import Verifier  :: *;
import RVFI_DII  :: *;

// ================================================================

   interface Piccolo_RVFI_DII_Bridge_IFC;
       interface RVFI_DII_Server #(XLEN) insr_inject;
       interface IMem_IFC insr_CPU;
       interface Put #(RVFI_DII_Execution #(XLEN)) trace_report;
   endinterface

   
   module mkPiccoloRVFIDIIBridge(Piccolo_RVFI_DII_Bridge_IFC);
       FIFOF#(Bit#(32)) requests <- mkGFIFOF(False,True);
       Reg#(Maybe#(WordXL)) fake_addr <- mkReg(tagged Invalid);
       FIFO#(RVFI_DII_Execution #(XLEN)) reports <- mkFIFO;

       FIFOF#(Bit#(32)) requests_backup <- mkUGSizedFIFOF(10);

       Reg#(UInt#(2)) pause_count[2] <- mkCReg(2, 0);
       Reg#(Bool) trapped[2] <- mkCReg(2, False);
       Reg#(Bool) instruction_pending <- mkReg(False);

       let internal_valid = (trapped[1] ? !instruction_pending && requests_backup.notEmpty && pause_count[1] == 0 : requests.notEmpty ) && isValid(fake_addr);
       let internal_instr = trapped[1] ? requests_backup.first : requests.first;

       rule rl_dec_pause_count;
           pause_count[0] <= pause_count[0] == 0 ? 0 : pause_count[0] - 1;
       endrule

       rule rl_esc_trap if (pause_count[1] == 0 && !instruction_pending && !requests_backup.notEmpty);
           trapped[0] <= False;
       endrule

       interface RVFI_DII_Server insr_inject;
           interface request = toPut(requests);
           interface response = toGet (reports);
       endinterface

       interface IMem_IFC insr_CPU;
           method Action req (Bit #(3) f3,
		       WordXL addr,
                       Bool   trap,
		       // The following  args for VM
		       Priv_Mode  priv,
		       Bit #(1)   sstatus_SUM,
		       Bit #(1)   mstatus_MXR,
		       WordXL     satp);
               if (internal_valid) begin
                   if (!trapped[1]) begin
                       if (trap) begin
                           pause_count[1] <= 3;
                           trapped[1] <= True;
                       end
                       requests_backup.enq(internal_instr);
                       requests.deq();
                   end
                   instruction_pending <= True;
               end
               fake_addr <= tagged Valid addr;
           endmethod

           method Bool valid = internal_valid;
           method WordXL pc = fromMaybe(?, fake_addr);
           method Instr instr = internal_instr;
           method Bool exc = False;
           method Exc_Code exc_code = 0;
       endinterface

       interface Put trace_report;
           method Action put (RVFI_DII_Execution #(XLEN) report);
               reports.enq(report);
               requests_backup.deq();
               instruction_pending <= False;
           endmethod
       endinterface
   endmodule

endpackage
