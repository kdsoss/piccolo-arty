// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2018 Jack Deeley (RVFI_DII)
//     Copyright (c) 2018-2019 Peter Rugg (RVFI_DII + CHERI)
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
//-

package CPU_Globals;

// ================================================================
// Types common to multiple CPU stages,
// including types communicated from stage to stage.

// ================================================================
// BSV library imports

// None

// ----------------
// BSV additional libs

// None

// ================================================================
// Project imports

import ISA_Decls :: *;

`ifdef ISA_CHERI
import CHERICap :: *;
import CHERICC_Fat :: *;
`endif

import TV_Info   :: *;

// ================================================================
// Output status of each stage

// EMPTY:   Stage has nothing in its input register
// BUSY:    Stage has input, but output is not ready
// PIPE:    Stage has input; driving normal output for pipeline
// NONPIPE: (In some stages) Stage has input; driving output is handled specially
//                (such as traps, CSR access, ...)

typedef enum {OSTATUS_EMPTY,
	      OSTATUS_BUSY,
	      OSTATUS_PIPE,
	      OSTATUS_NONPIPE
   } Stage_OStatus
deriving (Eq, Bits, FShow);

// ================================================================
// Bypass information
// From later to earlier stages.

// For an instruction's Rd (output GPR), a stage may:
// - have no Rd output
// - have Rd output, Rd is known but RdVal unknown
// - have Rd output, Rd is known and RdVal is known
// Note: a bypass has to stall if Rd matches and RdVal is unknown

typedef enum { BYPASS_RD_NONE, BYPASS_RD, BYPASS_RD_RDVAL } Bypass_State
deriving (Eq, Bits, FShow);

// We do not bypass CSR values, since we stall on CSRRxy insructions.

typedef struct {
   Bypass_State  bypass_state;
   RegName       rd;
`ifdef ISA_CHERI
   CapPipe       rd_val;
`else
   Word          rd_val;
`endif
   } Bypass
deriving (Bits);

instance FShow #(Bypass);
   function Fmt fshow (Bypass x);
      let fmt0 = $format ("Bypass {");
      let fmt1 = ((x.bypass_state == BYPASS_RD_NONE)
		  ? $format ("Rd -")
		  : $format ("Rd %0d ", x.rd) + ((x.bypass_state == BYPASS_RD)
						 ? $format ("-")
						 : $format ("rd_val:%h", x.rd_val)));
      let fmt2 = $format ("}");
      return fmt0 + fmt1 + fmt2;
   endfunction
endinstance

`ifdef ISA_F
typedef struct {
   Bypass_State  bypass_state;
   RegName       rd;
   WordFL        rd_val;
   } FBypass
deriving (Bits);

instance FShow #(FBypass);
   function Fmt fshow (FBypass x);
      let fmt0 = $format ("FBypass {");
      let fmt1 = ((x.bypass_state == BYPASS_RD_NONE)
		  ? $format ("FRd -")
		  : $format ("FRd %0d ", x.rd) + ((x.bypass_state == BYPASS_RD)
						 ? $format ("-")
						 : $format ("frd_val:%h", x.rd_val)));
      let fmt2 = $format ("}");
      return fmt0 + fmt1 + fmt2;
   endfunction
endinstance
`endif

// ----------------
// Baseline bypass info

Bypass no_bypass = Bypass {bypass_state: BYPASS_RD_NONE,
			   rd: ?,
			   rd_val: ? };

`ifdef ISA_F
FBypass no_fbypass = FBypass {bypass_state: BYPASS_RD_NONE,
			      rd: ?,
			      rd_val: ? };
`endif

// ----------------
// Bypass functions for GPRs
// Returns '(busy, val)'
// 'busy' means that the RegName is valid and matches, but the value is not available yet

`ifdef ISA_CHERI
function Tuple2 #(Bool, CapPipe) fn_gpr_bypass (Bypass bypass, RegName rd, CapPipe rd_val);
`else
function Tuple2 #(Bool, Word) fn_gpr_bypass (Bypass bypass, RegName rd, Word rd_val);
`endif
   Bool busy = ((bypass.bypass_state == BYPASS_RD) && (bypass.rd == rd));
   let val  = (  ((bypass.bypass_state == BYPASS_RD_RDVAL) && (bypass.rd == rd))
		? bypass.rd_val
		: rd_val);
   return tuple2 (busy, val);
endfunction

`ifdef ISA_F
// FBypass functions for FPRs
// Returns '(busy, val)'
// 'busy' means that the RegName is valid and matches, but the value is not available yet
function Tuple2 #(Bool, WordFL) fn_fpr_bypass (FBypass bypass, RegName rd, WordFL rd_val);
   Bool busy = ((bypass.bypass_state == BYPASS_RD) && (bypass.rd == rd));
   WordFL val= (  ((bypass.bypass_state == BYPASS_RD_RDVAL) && (bypass.rd == rd))
		? bypass.rd_val
		: rd_val);
   return tuple2 (busy, val);
endfunction
`endif

`ifdef ISA_CHERI
typeclass PCC#(type t);
    function Exact#(t) setPC (t oldPCC, Addr newPC);
    function Addr getPC (t pcc);
    function Bool checkPreValid (t pcc);
    function Maybe#(CHERI_Exc_Code) checkValid (t pcc, Bit#(TAdd#(XLEN,1)) top, Bool is_i32_not_i16);
    function t fromCapReg(CapReg pcc);
    function CapReg toCapReg(t pcc);
endtypeclass

instance PCC#(CapPipe);
    function Exact#(CapPipe) setPC (CapPipe oldPCC, Addr newPC);
        return setOffset(oldPCC, newPC, False);
    endfunction
    function Addr getPC (CapPipe pcc);
        return getOffset(pcc);
    endfunction
    // Check if a PCC dereference is valid before the instruction len is known
    function Bool checkPreValid (CapPipe pcc);
        //TODO alignment checks?
        return  isValidCap(pcc)
             && !isSealed(pcc)
             && getHardPerms(pcc).permitExecute
             && isInBounds(pcc, False);
    endfunction
    function Maybe#(CHERI_Exc_Code) checkValid (CapPipe pcc, Bit#(TAdd#(XLEN,1)) top, Bool is_i32_not_i16);
        let toRet = Invalid;
        //TODO alignment checks?
        if (!isValidCap(pcc))
            toRet = Valid(exc_code_CHERI_Tag);
        else if (isSealed(pcc))
            toRet = Valid(exc_code_CHERI_Seal);
        else if (!getHardPerms(pcc).permitExecute)
            toRet = Valid(exc_code_CHERI_XPerm);
        else if (zeroExtend(getAddr(pcc)) + (is_i32_not_i16 ? 4 : 2) > top)
            toRet = Valid(exc_code_CHERI_Length);
        return toRet;
    endfunction
    function CapPipe fromCapReg(CapReg pcc);
        return cast(pcc);
    endfunction
    function CapReg toCapReg(CapPipe pcc);
        return cast(pcc);
    endfunction
endinstance

typedef CapPipe PCC_T;

`endif

// ================================================================
// Trap information

typedef struct {
`ifdef ISA_CHERI
   PCC_T epcc;
   CapPipe eddc;
   CHERI_Exc_Code cheri_exc_code;
   Bit#(6) cheri_exc_reg;
`else
   Addr      epc;
`endif
   Exc_Code  exc_code;
   Addr      tval;
   } Trap_Info_Pipe
deriving (Bits, FShow);

// ================================================================
// Output from Stage 1

// Outputs from Stage1 to pipeline control
typedef enum {  CONTROL_STRAIGHT
	      , CONTROL_BRANCH
	      , CONTROL_CSRR_W
`ifdef ISA_CHERI
        , CONTROL_CAPBRANCH
	      , CONTROL_SCR_W
`endif
	      , CONTROL_CSRR_S_or_C
	      , CONTROL_FENCE
	      , CONTROL_FENCE_I
	      , CONTROL_SFENCE_VMA
	      , CONTROL_MRET
	      , CONTROL_SRET
	      , CONTROL_URET
	      , CONTROL_WFI
	      , CONTROL_TRAP
   } Control
deriving (Eq, Bits, FShow);

typedef struct {
   Stage_OStatus          ostatus;

   Control                control;

   Trap_Info_Pipe              trap_info;

   // feedback

`ifdef ISA_CHERI
   CapPipe                next_pcc;
   CapPipe                next_ddc;
`else
   WordXL                 next_pc;
`endif

   // feedforward data
   Data_Stage1_to_Stage2  data_to_stage2;
   } Output_Stage1
deriving (Bits);

instance FShow #(Output_Stage1);
   function Fmt fshow (Output_Stage1 x);
      Fmt fmt = $format ("Output_Stage1");
      if (x.ostatus == OSTATUS_EMPTY)
	 fmt = fmt + $format (" EMPTY");
      else if (x.ostatus == OSTATUS_BUSY)
	 fmt = fmt + $format (" BUSY pc:%h",
`ifdef ISA_CHERI
                                      getPC(x.data_to_stage2.pcc)
`else
                                      x.data_to_stage2.pc
`endif
                       );
      else begin
	 if (x.ostatus == OSTATUS_NONPIPE) begin
	    fmt = fmt + $format (" NONPIPE: pc:%h",
`ifdef ISA_CHERI
                                             getPC(x.data_to_stage2.pcc)
`else
                                             x.data_to_stage2.pc
`endif
                          );
	    fmt = fmt + $format (" ", fshow (x.control));
	    fmt = fmt + $format (" ", fshow (x.trap_info));
	 end
	 else
	    fmt = fmt + $format (" PIPE: ", fshow (x.control), " ", fshow (
`ifdef ISA_CHERI
                                      getPC(x.data_to_stage2.pcc)
`else
                                      x.data_to_stage2.pc
`endif
                                                                    ));

	 fmt = fmt + $format (" next_pc 0x%08h",
`ifdef ISA_CHERI
                                          getPC(x.next_pcc)
`else
                                          x.next_pc
`endif
                       );
      end
      return fmt;
   endfunction
endinstance

// ================================================================
// Data_Stage1_to_Stage2: Data output from Stage1 stage, input to DM stage

// Stage1 stage forwards, to DM, one of these 'opcodes'
// - ALU result (all non-mem, M and FD insructions)
// - DM request (Data Memory LD/ST/...)
// - Shifter Box request (SLL/SLLI, SRL/SRLI, SRA/SRAI)
// - MBox request (integer multiply/divide)
// - FDBox request (floating point ops)

typedef enum {  OP_Stage2_ALU         // Pass-through (non mem, M, FD, AMO)
	      , OP_Stage2_LD
	      , OP_Stage2_ST

`ifdef SHIFT_SERIAL
	      , OP_Stage2_SH
`endif

`ifdef ISA_M
	      , OP_Stage2_M
`endif

`ifdef ISA_A
	      , OP_Stage2_AMO
`endif

`ifdef ISA_F
	      , OP_Stage2_FD
`endif
   } Op_Stage2
deriving (Eq, Bits, FShow);

typedef struct {
   Priv_Mode  priv;
`ifdef ISA_CHERI
   PCC_T      pcc;
   CapPipe    ddc;
`else
   Addr       pc;
`endif
   Instr      instr;    // For debugging. Just funct3, funct7 are enough for
                        // functionality.
`ifdef RVFI_DII
   UInt#(SEQ_LEN) instr_seq;
`endif
   Op_Stage2  op_stage2;
   RegName    rd;
   Addr       addr;     // Branch, jump: newPC
                        // Mem ops and AMOs: mem addr
`ifdef ISA_D
   // When D is enabled, the val from Stage1 to Stage2 should be sized to
   // max (sizeOf (WordXL), sizeOf (WordFL))
   // Using lower-level Bit types here as the data in vals always be raw bit
   // data
   WordFL     val1;     // OP_Stage2_ALU: rd_val
                        // OP_Stage2_M and OP_Stage2_FD: arg1

   WordFL     val2;     // OP_Stage2_ST: store-val;
                        // OP_Stage2_M and OP_Stage2_FD: arg2
`elsif ISA_CHERI
   CapPipe    val1;     // OP_Stage2_ALU: rd_val
                        // OP_Stage2_M and OP_Stage2_FD: arg1

   CapPipe    val2;     // OP_Stage2_ST: store-val;
                        // OP_Stage2_M and OP_Stage2_FD: arg2
`else

   WordXL     val1;     // OP_Stage2_ALU: rd_val
                        // OP_Stage2_M and OP_Stage2_FD: arg1

   WordXL     val2;     // OP_Stage2_ST: store-val;
                        // OP_Stage2_M and OP_Stage2_FD: arg2
`endif

`ifdef ISA_CHERI
   // Bounds check: if check_enable, will test
   // address_low >= authority.base && address_high <? authority.top (?: strictness determined by check_inclusive value TODO)
   // TODO behaviour if address_low > address_high?
   // Does not check that authority is tagged, so only generates
   // Bounds exceptions
   CapPipe    check_authority;
   Bit#(6)    check_authority_idx;
   Bit#(XLEN)     check_address_low;
   Bit#(TAdd#(XLEN,1))     check_address_high;
   Bool       check_enable;
   Bool check_inclusive;

   Bool       mem_allow_cap;
`endif

   Bit#(3)    mem_width_code;
   Bool       mem_unsigned;

`ifdef ISA_F
   WordFL     val3;     // OP_Stage2_FD: arg3
   Bool       rd_in_fpr;// The rd should update into FPR
   Bit #(3)   rounding_mode;    // rounding mode from fcsr_frm or instr.rm
`endif

`ifdef INCLUDE_TANDEM_VERIF
   Trace_Data  trace_data;
`endif

`ifdef RVFI
   Data_RVFI_Stage1 info_RVFI_s1;
`endif
   } Data_Stage1_to_Stage2
deriving (Bits);

`ifdef RVFI

typedef struct {
    Bit#(ILEN)  instr;
    // From decode
    Bit#(5)     rs1_addr;
    Bit#(5)     rs2_addr;
    Bit#(XLEN)  rs1_data;
    Bit#(XLEN)  rs2_data;
    Bit#(XLEN)  pc_rdata;
    // TODO: Exceptions?
    Bit#(XLEN)  pc_wdata;
    // TODO: Needs 0'ing when unused?
    Bit#(MEMWIDTH)  mem_wdata;

    // From ALU:
    Bit#(5)     rd_addr;
    // Might be killed by memory OPs.
    Bool        rd_alu;
    Bit#(XLEN)  rd_wdata_alu;

    Bit#(XLEN)  mem_addr;

} Data_RVFI_Stage1 deriving (Bits, Eq);


`endif


instance FShow #(Data_Stage1_to_Stage2);
   function Fmt fshow (Data_Stage1_to_Stage2 x);
      Fmt fmt =   $format ("data_to_Stage 2 {pc:%h  instr:%h  priv:%0d\n",
`ifdef ISA_CHERI
                                                                           getPC(x.pcc)
`else
                                                                           x.pc
`endif
                                                                         , x.instr, x.priv);
      fmt = fmt + $format ("            op_stage2:", fshow (x.op_stage2), "  rd:%0d\n", x.rd);
`ifdef ISA_F
      fmt = fmt + $format ("            addr:%h  val1:%h  val2:%h  val3:%h}",
			   x.addr, x.val1, x.val2, x.val3);
`else
      fmt = fmt + $format ("            addr:%h  val1:%h  val2:%h}",
			   x.addr, x.val1, x.val2);
`endif
      return fmt;
   endfunction
endinstance

// ================================================================
// Output from Stage 2

typedef struct {
   Stage_OStatus          ostatus;
   Trap_Info_Pipe         trap_info;    // relevant if ostatus == OSTATUS_NONPIPE

`ifdef ISA_CHERI
   // Whether a capability bounds check succeeded
   Bool                   check_success;
`endif

   // feedback
   Bypass                 bypass;
`ifdef ISA_F
   FBypass                fbypass;
`endif

   // feedforward data
   Data_Stage2_to_Stage3  data_to_stage3;

   Trace_Data             trace_data;
   } Output_Stage2
deriving (Bits);

instance FShow #(Output_Stage2);
   function Fmt fshow (Output_Stage2 x);
      Fmt fmt = $format ("Output_Stage2");
      if (x.ostatus == OSTATUS_EMPTY)
	 fmt = fmt + $format (" EMPTY");
      else if (x.ostatus == OSTATUS_BUSY)
	 fmt = fmt + $format (" BUSY: pc:%0h", x.data_to_stage3.pc);
      else if (x.ostatus == OSTATUS_NONPIPE) begin
	 fmt = fmt + $format (" NONPIPE: ") + fshow (x.trap_info);
	 fmt = fmt + $format (" ") + fshow (x.trap_info);
      end
      else
	 fmt = fmt + $format (" PIPE: ") + fshow (x.data_to_stage3);
      return fmt;
   endfunction
endinstance

// ================================================================
// Data communicated from stage 2 to stage 3

typedef struct {
   Addr      pc;            // For debugging only
   Instr     instr;         // For debugging only
`ifdef RVFI_DII
   UInt#(SEQ_LEN) instr_seq;
`endif
   Priv_Mode priv;

   Bool      rd_valid;
   RegName   rd;

`ifdef RVFI
   Data_RVFI_Stage2 info_RVFI_s2;
`endif

`ifdef ISA_F
   Bool      upd_flags;
   Bool      rd_in_fpr;
   Bit #(5)  fpr_flags;
`endif

`ifdef ISA_D
   // When FP is enabled, the rd_val from Stage2 to Stage3 should be sized to
   // max (sizeOf (WordXL), sizeOf (WordFL))
   // Using lower-level Bit types here as the data in rd_val always be raw
   // bit data
   WordFL    rd_val;
`elsif ISA_CHERI
   CapPipe   rd_val;
`else
   WordXL    rd_val;
`endif
   } Data_Stage2_to_Stage3
deriving (Bits);

`ifdef RVFI

typedef struct {
    Data_RVFI_Stage1    stage1;
    // Hard to know what was written as SC pretends to write "0" on failure
    // instead of actual untouched value. So, indicate wmask = 0 perhaps?

    Bit#(TDiv#(MEMWIDTH,8))       mem_rmask;
    Bit#(TDiv#(MEMWIDTH,8))       mem_wmask;

}   Data_RVFI_Stage2 deriving (Bits);

`endif

instance FShow #(Data_Stage2_to_Stage3);
   function Fmt fshow (Data_Stage2_to_Stage3 x);
      Fmt fmt =   $format ("data_to_Stage3 {pc:%h  instr:%h  priv:%0d\n", x.pc, x.instr, x.priv);
      fmt = fmt + $format ("        rd_valid:", fshow (x.rd_valid));

`ifdef ISA_F
      if (x.upd_flags)
         fmt = fmt + $format ("  fflags: %05b", fshow (x.fpr_flags));

      if (x.rd_in_fpr)
         fmt = fmt + $format ("  frd:%0d  rd_val:%h\n", x.rd, x.rd_val);
      else
`endif
         fmt = fmt + $format ("  grd:%0d  rd_val:%h\n", x.rd, x.rd_val);
      return fmt;
   endfunction
endinstance

// ================================================================
// Output from Stage 3

typedef struct {
   Stage_OStatus  ostatus;
   Bypass         bypass;
`ifdef ISA_F
   FBypass        fbypass;
`endif
   } Output_Stage3
deriving (Bits);

instance FShow #(Output_Stage3);
   function Fmt fshow (Output_Stage3 x);
      Fmt fmt = $format ("Output_Stage3");
      if (x.ostatus == OSTATUS_EMPTY)
	 fmt = fmt + $format (" EMPTY");
      else if (x.ostatus == OSTATUS_BUSY)
	 fmt = fmt + $format (" BUSY");
      else if (x.ostatus == OSTATUS_PIPE)
	 fmt = fmt + $format (" PIPE");
      else if (x.ostatus == OSTATUS_NONPIPE)
	 fmt = fmt + $format (" NONPIPE");
      return fmt;
   endfunction
endinstance

// ================================================================

endpackage
