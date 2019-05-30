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

function Bit #(5) instr_cap_funct5rs2 (Instr x); return x [24:20]; endfunction

function Bit #(5) instr_cap_funct5rd (Instr x); return x [11:7]; endfunction

function Instr mkInstr_Cap_type (Bit #(7) funct7, Bit #(5) funct5rs2, RegName rs1, Bit #(3) funct3, RegName rd, Bit #(7) opcode);
    let instr = { funct7, funct5rs2, rs1, funct3, rd, opcode };
    return instr;
endfunction

function Instr mkInstr_Cap_Store_type (Bit#(7) funct7, RegName rs2, RegName rs1, Bit #(3) funct3, Bit #(5) funct5rd, Bit #(7) opcode);
    let inst = { funct7, rs2, rs1, funct3, funct5rd, opcode };
    return inst;
endfunction

// Exception codes

Exc_Code exc_code_CHERI = 32;

typedef Bit #(5) CHERI_Exc_Code;

Exc_Code exc_code_CHERI_None         = 0;
Exc_Code exc_code_CHERI_Length       = 1;
Exc_Code exc_code_CHERI_Tag          = 2;
Exc_Code exc_code_CHERI_Seal         = 3;
Exc_Code exc_code_CHERI_Type         = 4;
Exc_Code exc_code_CHERI_Call         = 5;
Exc_Code exc_code_CHERI_Return       = 6;
Exc_Code exc_code_CHERI_Underflow    = 7;
Exc_Code exc_code_CHERI_Software     = 8;
Exc_Code exc_code_CHERI_TLB          = 9;
Exc_Code exc_code_CHERI_Precision    = 10;
Exc_Code exc_code_CHERI_Global       = 16;
Exc_Code exc_code_CHERI_XPerm        = 17;
Exc_Code exc_code_CHERI_RPerm        = 18;
Exc_Code exc_code_CHERI_WPerm        = 19;
Exc_Code exc_code_CHERI_LCPerm       = 20;
Exc_Code exc_code_CHERI_SCPerm       = 21;
Exc_Code exc_code_CHERI_SCLocalPerm  = 22;
Exc_Code exc_code_CHERI_SealPerm     = 23;
Exc_Code exc_code_CHERI_SysRegsPerm  = 24;
Exc_Code exc_code_CHERI_CCallPerm    = 25;
Exc_Code exc_code_CHERI_CCallIDCPerm = 26;
Exc_Code exc_code_CHERI_UnsealPerm   = 27;

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

// 7'h00 unused
Bit #(7) f7_cap_CSpecialRW      = 7'h01;
// 7'h02-7'h07 unused
Bit #(7) f7_cap_CSetBounds      = 7'h08;
Bit #(7) f7_cap_CSetBoundsExact = 7'h09;
// 7'h0a unused
Bit #(7) f7_cap_CSeal           = 7'h0b;
Bit #(7) f7_cap_CUnseal         = 7'h0c;
Bit #(7) f7_cap_CAndPerm        = 7'h0d;
Bit #(7) f7_cap_CSetFlags       = 7'h0e;
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
// 7'h21-7'hfb unused
Bit #(7) f7_cap_Stores          = 7'h7c;
Bit #(7) f7_cap_Loads           = 7'h7d;
Bit #(7) f7_cap_CCall           = 7'h7e;
Bit #(7) f7_cap_TwoOp           = 7'h7f;

// ================================================================
// f7_cap_TwoOp opcode subdivision

// f5rs2 selects between 2-reg operations (f5rs2 instead of f5 because f5
//        is already used in RISC-V and is in a different position

Bit #(5) f5rs2_cap_CGetPerm    = 5'h00;
Bit #(5) f5rs2_cap_CGetType    = 5'h01;
Bit #(5) f5rs2_cap_CGetBase    = 5'h02;
Bit #(5) f5rs2_cap_CGetLen     = 5'h03;
Bit #(5) f5rs2_cap_CGetTag     = 5'h04;
Bit #(5) f5rs2_cap_CGetSealed  = 5'h05;
Bit #(5) f5rs2_cap_CGetOffset  = 5'h06;
Bit #(5) f5rs2_cap_CGetFlags   = 5'h07;
// 5'h08-5'h09 unused
Bit #(5) f5rs2_cap_CMove       = 5'h0a;
Bit #(5) f5rs2_cap_CClearTag   = 5'h0b;
Bit #(5) f5rs2_cap_CJALR       = 5'h0c;
Bit #(5) f5rs2_cap_CClearReg   = 5'h0d;
// 5'h0e unused
Bit #(5) f5rs2_cap_CGetAddr    = 5'h0f;
Bit #(5) f5rs2_cap_CClearFPReg = 5'h10;
// 5'h11-5'h1f unused (5'h1f reserved for 1-reg instructions

// ================================================================
// f7_cap_{Load, Store} opcode subdivision

MemReqSize cap_mem_SIZE_B = 2'h0;
MemReqSize cap_mem_SIZE_H = 2'h1;
MemReqSize cap_mem_SIZE_W = 2'h2;
MemReqSize cap_mem_SIZE_D = 2'h3;
//MemReqSize f5rs2_cap_mem_SIZE_Q = 2'h4; //TODO

Bit #(1) cap_mem_ddc = 1'h0;
Bit #(1) cap_mem_cap = 1'h1;

Bit #(1) cap_mem_unsigned = 1'h1;
Bit #(1) cap_mem_signed = 1'h0;

// ================================================================
// Other:

// Region in MISC_MEM for LQ
Bit #(3) f3_LQ = 3'h2;

`endif
