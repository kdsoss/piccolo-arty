/*
 * Copyright (c) 2019 Peter Rugg
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


`ifdef ISA_CHERI

function Bit #(5) instr_cap_funct5c (Instr x); return x [24:20]; endfunction

function Bit #(5) instr_cap_imm5 (Instr x); return x [24:20]; endfunction

function Instr mkInstr_Cap_type (Bit #(7) funct7, Bit #(5) funct5c, RegName rs1, Bit #(3) funct3, RegName rd, Bit #(7) opcode);
    let instr = { funct7, funct5c, rs1, funct3, rd, opcode };
    return instr;
endfunction

function Instr mkInstr_Cap_Mod_type (Bit #(7) funct7, Bit #(5) imm5, RegName rs1, Bit #(3) funct3, RegName rd, Bit #(7) opcode);
    let inst = { funct7, imm5, rs1, funct3, rd, opcode };
    return inst;
endfunction

// Instruction field encodings

// Top-level opcodes
Opcode   op_cap_Manip = 7'h5b;
//Opcode   op_cap_Mem   = 7'h0b; // Not yet implemented

// ================================================================
// op_cap_Manip opcode subdivision

// f3 selects between immediate and 3-reg instructions
Bit #(3) f3_cap_ThreeOp             = 3'h0;
Bit #(3) f3_cap_CIncOffsetImmediate = 3'h1;
Bit #(3) f3_cap_CSetBoundsImmediate = 3'h2;
// 3'h3-3'h7 unused

// ================================================================
// op_cap_ThreeOp opcode subdivision

// f7 selects between 3-reg operations

Bit #(7) f7_cap_Mem             = 7'h00;
Bit #(7) f7_cap_CSpecialRW      = 7'h01;
// 7'h02-7'h07 unused
Bit #(7) f7_cap_CSetBounds      = 7'h08;
Bit #(7) f7_cap_CSetBoundsExact = 7'h09;
// 7'h0a unused
Bit #(7) f7_cap_CSeal           = 7'h0b;
Bit #(7) f7_cap_CUnseal         = 7'h0c;
Bit #(7) f7_cap_CAndPerm        = 7'h0d;
// 7'h0e unused
Bit #(7) f7_cap_CSetOffset      = 7'h0f;
// 7'h10 unused
Bit #(7) f7_cap_CIncOffset      = 7'h11;
Bit #(7) f7_cap_CToPtr          = 7'h12;
Bit #(7) f7_cap_CFromPtr        = 7'h13;
// 7'h14-7'h1c unused
Bit #(7) f7_cap_CBuildCap       = 7'h1d;
Bit #(7) f7_cap_CCopyType       = 7'h1e;
Bit #(7) f7_cap_CCSeal          = 7'h1f;
Bit #(7) f7_cap_CTestSubset     = 7'h20;
// 7'h21-7'hfd unused
Bit #(7) f7_cap_CCall           = 7'h7e;
Bit #(7) f7_cap_TwoOp           = 7'h7f;

// ================================================================
// f7_cap_TwoOp opcode subdivision

// f5c selects between 2-reg operations (f5c instead of f5 because f5
//        is already used in RISC-V and is in a different position

Bit #(5) f5c_cap_CGetPerm    = 5'h00;
Bit #(5) f5c_cap_CGetType    = 5'h01;
Bit #(5) f5c_cap_CGetBase    = 5'h02;
Bit #(5) f5c_cap_CGetLen     = 5'h03;
Bit #(5) f5c_cap_CGetTag     = 5'h04;
Bit #(5) f5c_cap_CGetSealed  = 5'h05;
Bit #(5) f5c_cap_CGetOffset  = 5'h06;
// 5'h07 unused
Bit #(5) f5c_cap_CCheckPerm  = 5'h08;
Bit #(5) f5c_cap_CCheckType  = 5'h09;
Bit #(5) f5c_cap_CMove       = 5'h0a;
Bit #(5) f5c_cap_CClearTag   = 5'h0b;
Bit #(5) f5c_cap_CJALR       = 5'h0c;
Bit #(5) f5c_cap_CClearReg   = 5'h0d;
// 5'h0e unused
Bit #(5) f5c_cap_CGetAddr    = 5'h0f;
Bit #(5) f5c_cap_CClearFPReg = 5'h10;
// 5'h11-5'h1f unused (5'h1f reserved for 1-reg instructions

// ================================================================
// f7_cap_Mem opcode subdivision

MemReqSize f5c_cap_mem_SIZE_B = 2'h0;
MemReqSize f5c_cap_mem_SIZE_H = 2'h1;
MemReqSize f5c_cap_mem_SIZE_W = 2'h2;
MemReqSize f5c_cap_mem_SIZE_D = 2'h3;

Bit #(1) f5c_cap_mem_LOAD  = 1'h0;
Bit #(1) f5c_cap_mem_STORE = 1'h1;

Bit #(1) f5c_cap_mem_ddc = 1'h0;
Bit #(1) f5c_cap_mem_cap = 1'h1;

Bit #(5) f5c_cap_mem_LBddc  = 5'h00;
Bit #(5) f5c_cap_mem_LHddc  = 5'h01;
Bit #(5) f5c_cap_mem_LWddc  = 5'h02;
Bit #(5) f5c_cap_mem_LDddc  = 5'h03;
Bit #(5) f5c_cap_mem_LBUddc = 5'h04;
Bit #(5) f5c_cap_mem_LHUddc = 5'h05;
Bit #(5) f5c_cap_mem_LWUddc = 5'h06;
Bit #(5) f5c_cap_mem_LDUddc = 5'h07;
Bit #(5) f5c_cap_mem_SBddc  = 5'h08;
Bit #(5) f5c_cap_mem_SHddc  = 5'h09;
Bit #(5) f5c_cap_mem_SWddc  = 5'h0a;
Bit #(5) f5c_cap_mem_SDddc  = 5'h0b;
Bit #(5) f5c_cap_mem_SQddc  = 5'h0c;
Bit #(5) f5c_cap_mem_LQddc  = 5'h0d;
// 5'h0e-5'h0f unused
Bit #(5) f5c_cap_mem_LBcap  = 5'h10;
Bit #(5) f5c_cap_mem_LHcap  = 5'h11;
Bit #(5) f5c_cap_mem_LWcap  = 5'h12;
Bit #(5) f5c_cap_mem_LDcap  = 5'h13;
Bit #(5) f5c_cap_mem_LBUcap = 5'h14;
Bit #(5) f5c_cap_mem_LHUcap = 5'h15;
Bit #(5) f5c_cap_mem_LWUcap = 5'h16;
Bit #(5) f5c_cap_mem_LDUcap = 5'h17;
Bit #(5) f5c_cap_mem_SBcap  = 5'h18;
Bit #(5) f5c_cap_mem_SHcap  = 5'h19;
Bit #(5) f5c_cap_mem_SWcap  = 5'h1a;
Bit #(5) f5c_cap_mem_SDcap  = 5'h1b;
Bit #(5) f5c_cap_mem_SQcap  = 5'h1c;
Bit #(5) f5c_cap_mem_LQcap  = 5'h1d;
// 5'h1e-5'h1f unused

`endif
