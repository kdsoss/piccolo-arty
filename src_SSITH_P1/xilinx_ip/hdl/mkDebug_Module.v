//
// Generated by Bluespec Compiler, version untagged-gad02e931 (build ad02e931)
//
// On Fri Jul 23 15:57:50 EEST 2021
//
//
// Ports:
// Name                         I/O  size props
// RDY_dmi_read_addr              O     1
// dmi_read_data                  O    32
// RDY_dmi_read_data              O     1
// RDY_dmi_write                  O     1
// hart0_reset_client_request_get  O     1 reg
// RDY_hart0_reset_client_request_get  O     1 reg
// RDY_hart0_reset_client_response_put  O     1 reg
// hart0_client_run_halt_request_get  O     1 reg
// RDY_hart0_client_run_halt_request_get  O     1 reg
// RDY_hart0_client_run_halt_response_put  O     1 reg
// hart0_get_other_req_get        O     4 reg
// RDY_hart0_get_other_req_get    O     1 reg
// hart0_gpr_mem_client_request_get  O    38 reg
// RDY_hart0_gpr_mem_client_request_get  O     1 reg
// RDY_hart0_gpr_mem_client_response_put  O     1 reg
// hart0_csr_mem_client_request_get  O    45 reg
// RDY_hart0_csr_mem_client_request_get  O     1 reg
// RDY_hart0_csr_mem_client_response_put  O     1 reg
// ndm_reset_client_request_get   O     1 reg
// RDY_ndm_reset_client_request_get  O     1 reg
// RDY_ndm_reset_client_response_put  O     1 reg
// master_aw_canPeek              O     1 reg
// master_aw_peek                 O    97 reg
// RDY_master_aw_peek             O     1 reg
// RDY_master_aw_drop             O     1 reg
// master_w_canPeek               O     1 reg
// master_w_peek                  O    74 reg
// RDY_master_w_peek              O     1 reg
// RDY_master_w_drop              O     1 reg
// master_b_canPut                O     1 reg
// RDY_master_b_put               O     1 reg
// master_ar_canPeek              O     1 reg
// master_ar_peek                 O    97 reg
// RDY_master_ar_peek             O     1 reg
// RDY_master_ar_drop             O     1 reg
// master_r_canPut                O     1 reg
// RDY_master_r_put               O     1 reg
// CLK                            I     1 clock
// RST_N                          I     1 reset
// dmi_read_addr_dm_addr          I     7 reg
// dmi_write_dm_addr              I     7
// dmi_write_dm_word              I    32
// hart0_reset_client_response_put  I     1 reg
// hart0_client_run_halt_response_put  I     1 reg
// hart0_gpr_mem_client_response_put  I    33 reg
// hart0_csr_mem_client_response_put  I    33 reg
// ndm_reset_client_response_put  I     1 reg
// master_b_put_val               I     6 reg
// master_r_put_val               I    72 reg
// EN_dmi_read_addr               I     1
// EN_dmi_write                   I     1
// EN_hart0_reset_client_response_put  I     1
// EN_hart0_client_run_halt_response_put  I     1
// EN_hart0_gpr_mem_client_response_put  I     1
// EN_hart0_csr_mem_client_response_put  I     1
// EN_ndm_reset_client_response_put  I     1
// EN_master_aw_drop              I     1
// EN_master_w_drop               I     1
// EN_master_b_put                I     1
// EN_master_ar_drop              I     1
// EN_master_r_put                I     1
// EN_dmi_read_data               I     1
// EN_hart0_reset_client_request_get  I     1
// EN_hart0_client_run_halt_request_get  I     1
// EN_hart0_get_other_req_get     I     1
// EN_hart0_gpr_mem_client_request_get  I     1
// EN_hart0_csr_mem_client_request_get  I     1
// EN_ndm_reset_client_request_get  I     1
//
// Combinational paths from inputs to outputs:
//   EN_dmi_read_data -> dmi_read_data
//
//

`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif

module mkDebug_Module(CLK,
		      RST_N,

		      dmi_read_addr_dm_addr,
		      EN_dmi_read_addr,
		      RDY_dmi_read_addr,

		      EN_dmi_read_data,
		      dmi_read_data,
		      RDY_dmi_read_data,

		      dmi_write_dm_addr,
		      dmi_write_dm_word,
		      EN_dmi_write,
		      RDY_dmi_write,

		      EN_hart0_reset_client_request_get,
		      hart0_reset_client_request_get,
		      RDY_hart0_reset_client_request_get,

		      hart0_reset_client_response_put,
		      EN_hart0_reset_client_response_put,
		      RDY_hart0_reset_client_response_put,

		      EN_hart0_client_run_halt_request_get,
		      hart0_client_run_halt_request_get,
		      RDY_hart0_client_run_halt_request_get,

		      hart0_client_run_halt_response_put,
		      EN_hart0_client_run_halt_response_put,
		      RDY_hart0_client_run_halt_response_put,

		      EN_hart0_get_other_req_get,
		      hart0_get_other_req_get,
		      RDY_hart0_get_other_req_get,

		      EN_hart0_gpr_mem_client_request_get,
		      hart0_gpr_mem_client_request_get,
		      RDY_hart0_gpr_mem_client_request_get,

		      hart0_gpr_mem_client_response_put,
		      EN_hart0_gpr_mem_client_response_put,
		      RDY_hart0_gpr_mem_client_response_put,

		      EN_hart0_csr_mem_client_request_get,
		      hart0_csr_mem_client_request_get,
		      RDY_hart0_csr_mem_client_request_get,

		      hart0_csr_mem_client_response_put,
		      EN_hart0_csr_mem_client_response_put,
		      RDY_hart0_csr_mem_client_response_put,

		      EN_ndm_reset_client_request_get,
		      ndm_reset_client_request_get,
		      RDY_ndm_reset_client_request_get,

		      ndm_reset_client_response_put,
		      EN_ndm_reset_client_response_put,
		      RDY_ndm_reset_client_response_put,

		      master_aw_canPeek,

		      master_aw_peek,
		      RDY_master_aw_peek,

		      EN_master_aw_drop,
		      RDY_master_aw_drop,

		      master_w_canPeek,

		      master_w_peek,
		      RDY_master_w_peek,

		      EN_master_w_drop,
		      RDY_master_w_drop,

		      master_b_canPut,

		      master_b_put_val,
		      EN_master_b_put,
		      RDY_master_b_put,

		      master_ar_canPeek,

		      master_ar_peek,
		      RDY_master_ar_peek,

		      EN_master_ar_drop,
		      RDY_master_ar_drop,

		      master_r_canPut,

		      master_r_put_val,
		      EN_master_r_put,
		      RDY_master_r_put);
  input  CLK;
  input  RST_N;

  // action method dmi_read_addr
  input  [6 : 0] dmi_read_addr_dm_addr;
  input  EN_dmi_read_addr;
  output RDY_dmi_read_addr;

  // actionvalue method dmi_read_data
  input  EN_dmi_read_data;
  output [31 : 0] dmi_read_data;
  output RDY_dmi_read_data;

  // action method dmi_write
  input  [6 : 0] dmi_write_dm_addr;
  input  [31 : 0] dmi_write_dm_word;
  input  EN_dmi_write;
  output RDY_dmi_write;

  // actionvalue method hart0_reset_client_request_get
  input  EN_hart0_reset_client_request_get;
  output hart0_reset_client_request_get;
  output RDY_hart0_reset_client_request_get;

  // action method hart0_reset_client_response_put
  input  hart0_reset_client_response_put;
  input  EN_hart0_reset_client_response_put;
  output RDY_hart0_reset_client_response_put;

  // actionvalue method hart0_client_run_halt_request_get
  input  EN_hart0_client_run_halt_request_get;
  output hart0_client_run_halt_request_get;
  output RDY_hart0_client_run_halt_request_get;

  // action method hart0_client_run_halt_response_put
  input  hart0_client_run_halt_response_put;
  input  EN_hart0_client_run_halt_response_put;
  output RDY_hart0_client_run_halt_response_put;

  // actionvalue method hart0_get_other_req_get
  input  EN_hart0_get_other_req_get;
  output [3 : 0] hart0_get_other_req_get;
  output RDY_hart0_get_other_req_get;

  // actionvalue method hart0_gpr_mem_client_request_get
  input  EN_hart0_gpr_mem_client_request_get;
  output [37 : 0] hart0_gpr_mem_client_request_get;
  output RDY_hart0_gpr_mem_client_request_get;

  // action method hart0_gpr_mem_client_response_put
  input  [32 : 0] hart0_gpr_mem_client_response_put;
  input  EN_hart0_gpr_mem_client_response_put;
  output RDY_hart0_gpr_mem_client_response_put;

  // actionvalue method hart0_csr_mem_client_request_get
  input  EN_hart0_csr_mem_client_request_get;
  output [44 : 0] hart0_csr_mem_client_request_get;
  output RDY_hart0_csr_mem_client_request_get;

  // action method hart0_csr_mem_client_response_put
  input  [32 : 0] hart0_csr_mem_client_response_put;
  input  EN_hart0_csr_mem_client_response_put;
  output RDY_hart0_csr_mem_client_response_put;

  // actionvalue method ndm_reset_client_request_get
  input  EN_ndm_reset_client_request_get;
  output ndm_reset_client_request_get;
  output RDY_ndm_reset_client_request_get;

  // action method ndm_reset_client_response_put
  input  ndm_reset_client_response_put;
  input  EN_ndm_reset_client_response_put;
  output RDY_ndm_reset_client_response_put;

  // value method master_aw_canPeek
  output master_aw_canPeek;

  // value method master_aw_peek
  output [96 : 0] master_aw_peek;
  output RDY_master_aw_peek;

  // action method master_aw_drop
  input  EN_master_aw_drop;
  output RDY_master_aw_drop;

  // value method master_w_canPeek
  output master_w_canPeek;

  // value method master_w_peek
  output [73 : 0] master_w_peek;
  output RDY_master_w_peek;

  // action method master_w_drop
  input  EN_master_w_drop;
  output RDY_master_w_drop;

  // value method master_b_canPut
  output master_b_canPut;

  // action method master_b_put
  input  [5 : 0] master_b_put_val;
  input  EN_master_b_put;
  output RDY_master_b_put;

  // value method master_ar_canPeek
  output master_ar_canPeek;

  // value method master_ar_peek
  output [96 : 0] master_ar_peek;
  output RDY_master_ar_peek;

  // action method master_ar_drop
  input  EN_master_ar_drop;
  output RDY_master_ar_drop;

  // value method master_r_canPut
  output master_r_canPut;

  // action method master_r_put
  input  [71 : 0] master_r_put_val;
  input  EN_master_r_put;
  output RDY_master_r_put;

  // signals for module outputs
  reg [31 : 0] dmi_read_data;
  wire [96 : 0] master_ar_peek, master_aw_peek;
  wire [73 : 0] master_w_peek;
  wire [44 : 0] hart0_csr_mem_client_request_get;
  wire [37 : 0] hart0_gpr_mem_client_request_get;
  wire [3 : 0] hart0_get_other_req_get;
  wire RDY_dmi_read_addr,
       RDY_dmi_read_data,
       RDY_dmi_write,
       RDY_hart0_client_run_halt_request_get,
       RDY_hart0_client_run_halt_response_put,
       RDY_hart0_csr_mem_client_request_get,
       RDY_hart0_csr_mem_client_response_put,
       RDY_hart0_get_other_req_get,
       RDY_hart0_gpr_mem_client_request_get,
       RDY_hart0_gpr_mem_client_response_put,
       RDY_hart0_reset_client_request_get,
       RDY_hart0_reset_client_response_put,
       RDY_master_ar_drop,
       RDY_master_ar_peek,
       RDY_master_aw_drop,
       RDY_master_aw_peek,
       RDY_master_b_put,
       RDY_master_r_put,
       RDY_master_w_drop,
       RDY_master_w_peek,
       RDY_ndm_reset_client_request_get,
       RDY_ndm_reset_client_response_put,
       hart0_client_run_halt_request_get,
       hart0_reset_client_request_get,
       master_ar_canPeek,
       master_aw_canPeek,
       master_b_canPut,
       master_r_canPut,
       master_w_canPeek,
       ndm_reset_client_request_get;

  // ports of submodule dm_abstract_commands
  wire [44 : 0] dm_abstract_commands$hart0_csr_mem_client_request_get;
  wire [37 : 0] dm_abstract_commands$hart0_gpr_mem_client_request_get;
  wire [32 : 0] dm_abstract_commands$hart0_csr_mem_client_response_put,
		dm_abstract_commands$hart0_gpr_mem_client_response_put;
  wire [31 : 0] dm_abstract_commands$av_read,
		dm_abstract_commands$write_dm_word;
  wire [6 : 0] dm_abstract_commands$av_read_dm_addr,
	       dm_abstract_commands$write_dm_addr;
  wire dm_abstract_commands$EN_av_read,
       dm_abstract_commands$EN_hart0_csr_mem_client_request_get,
       dm_abstract_commands$EN_hart0_csr_mem_client_response_put,
       dm_abstract_commands$EN_hart0_gpr_mem_client_request_get,
       dm_abstract_commands$EN_hart0_gpr_mem_client_response_put,
       dm_abstract_commands$EN_reset,
       dm_abstract_commands$EN_write,
       dm_abstract_commands$RDY_hart0_csr_mem_client_request_get,
       dm_abstract_commands$RDY_hart0_csr_mem_client_response_put,
       dm_abstract_commands$RDY_hart0_gpr_mem_client_request_get,
       dm_abstract_commands$RDY_hart0_gpr_mem_client_response_put;

  // ports of submodule dm_run_control
  wire [31 : 0] dm_run_control$av_read, dm_run_control$write_dm_word;
  wire [6 : 0] dm_run_control$av_read_dm_addr, dm_run_control$write_dm_addr;
  wire [3 : 0] dm_run_control$hart0_get_other_req_get;
  wire dm_run_control$EN_av_read,
       dm_run_control$EN_hart0_client_run_halt_request_get,
       dm_run_control$EN_hart0_client_run_halt_response_put,
       dm_run_control$EN_hart0_get_other_req_get,
       dm_run_control$EN_hart0_reset_client_request_get,
       dm_run_control$EN_hart0_reset_client_response_put,
       dm_run_control$EN_ndm_reset_client_request_get,
       dm_run_control$EN_ndm_reset_client_response_put,
       dm_run_control$EN_reset,
       dm_run_control$EN_write,
       dm_run_control$RDY_hart0_client_run_halt_request_get,
       dm_run_control$RDY_hart0_client_run_halt_response_put,
       dm_run_control$RDY_hart0_get_other_req_get,
       dm_run_control$RDY_hart0_reset_client_request_get,
       dm_run_control$RDY_hart0_reset_client_response_put,
       dm_run_control$RDY_ndm_reset_client_request_get,
       dm_run_control$RDY_ndm_reset_client_response_put,
       dm_run_control$RDY_write,
       dm_run_control$dmactive,
       dm_run_control$hart0_client_run_halt_request_get,
       dm_run_control$hart0_client_run_halt_response_put,
       dm_run_control$hart0_reset_client_request_get,
       dm_run_control$hart0_reset_client_response_put,
       dm_run_control$ndm_reset_client_request_get,
       dm_run_control$ndm_reset_client_response_put;

  // ports of submodule dm_system_bus
  wire [96 : 0] dm_system_bus$master_ar_peek, dm_system_bus$master_aw_peek;
  wire [73 : 0] dm_system_bus$master_w_peek;
  wire [71 : 0] dm_system_bus$master_r_put_val;
  wire [31 : 0] dm_system_bus$av_read, dm_system_bus$write_dm_word;
  wire [6 : 0] dm_system_bus$av_read_dm_addr, dm_system_bus$write_dm_addr;
  wire [5 : 0] dm_system_bus$master_b_put_val;
  wire dm_system_bus$EN_av_read,
       dm_system_bus$EN_master_ar_drop,
       dm_system_bus$EN_master_aw_drop,
       dm_system_bus$EN_master_b_put,
       dm_system_bus$EN_master_r_put,
       dm_system_bus$EN_master_w_drop,
       dm_system_bus$EN_reset,
       dm_system_bus$EN_write,
       dm_system_bus$RDY_av_read,
       dm_system_bus$RDY_master_ar_drop,
       dm_system_bus$RDY_master_ar_peek,
       dm_system_bus$RDY_master_aw_drop,
       dm_system_bus$RDY_master_aw_peek,
       dm_system_bus$RDY_master_b_put,
       dm_system_bus$RDY_master_r_put,
       dm_system_bus$RDY_master_w_drop,
       dm_system_bus$RDY_master_w_peek,
       dm_system_bus$RDY_write,
       dm_system_bus$master_ar_canPeek,
       dm_system_bus$master_aw_canPeek,
       dm_system_bus$master_b_canPut,
       dm_system_bus$master_r_canPut,
       dm_system_bus$master_w_canPeek;

  // ports of submodule f_read_addr
  wire [6 : 0] f_read_addr$D_IN, f_read_addr$D_OUT;
  wire f_read_addr$CLR,
       f_read_addr$DEQ,
       f_read_addr$EMPTY_N,
       f_read_addr$ENQ,
       f_read_addr$FULL_N;

  // rule scheduling signals
  wire CAN_FIRE_RL_rl_reset,
       CAN_FIRE_dmi_read_addr,
       CAN_FIRE_dmi_read_data,
       CAN_FIRE_dmi_write,
       CAN_FIRE_hart0_client_run_halt_request_get,
       CAN_FIRE_hart0_client_run_halt_response_put,
       CAN_FIRE_hart0_csr_mem_client_request_get,
       CAN_FIRE_hart0_csr_mem_client_response_put,
       CAN_FIRE_hart0_get_other_req_get,
       CAN_FIRE_hart0_gpr_mem_client_request_get,
       CAN_FIRE_hart0_gpr_mem_client_response_put,
       CAN_FIRE_hart0_reset_client_request_get,
       CAN_FIRE_hart0_reset_client_response_put,
       CAN_FIRE_master_ar_drop,
       CAN_FIRE_master_aw_drop,
       CAN_FIRE_master_b_put,
       CAN_FIRE_master_r_put,
       CAN_FIRE_master_w_drop,
       CAN_FIRE_ndm_reset_client_request_get,
       CAN_FIRE_ndm_reset_client_response_put,
       WILL_FIRE_RL_rl_reset,
       WILL_FIRE_dmi_read_addr,
       WILL_FIRE_dmi_read_data,
       WILL_FIRE_dmi_write,
       WILL_FIRE_hart0_client_run_halt_request_get,
       WILL_FIRE_hart0_client_run_halt_response_put,
       WILL_FIRE_hart0_csr_mem_client_request_get,
       WILL_FIRE_hart0_csr_mem_client_response_put,
       WILL_FIRE_hart0_get_other_req_get,
       WILL_FIRE_hart0_gpr_mem_client_request_get,
       WILL_FIRE_hart0_gpr_mem_client_response_put,
       WILL_FIRE_hart0_reset_client_request_get,
       WILL_FIRE_hart0_reset_client_response_put,
       WILL_FIRE_master_ar_drop,
       WILL_FIRE_master_aw_drop,
       WILL_FIRE_master_b_put,
       WILL_FIRE_master_r_put,
       WILL_FIRE_master_w_drop,
       WILL_FIRE_ndm_reset_client_request_get,
       WILL_FIRE_ndm_reset_client_response_put;

  // declarations used by system tasks
  // synopsys translate_off
  reg [31 : 0] v__h485;
  reg [31 : 0] v__h479;
  // synopsys translate_on

  // action method dmi_read_addr
  assign RDY_dmi_read_addr = dm_run_control$dmactive && f_read_addr$FULL_N ;
  assign CAN_FIRE_dmi_read_addr =
	     dm_run_control$dmactive && f_read_addr$FULL_N ;
  assign WILL_FIRE_dmi_read_addr = EN_dmi_read_addr ;

  // actionvalue method dmi_read_data
  always@(f_read_addr$D_OUT or
	  dm_abstract_commands$av_read or
	  dm_run_control$av_read or dm_system_bus$av_read)
  begin
    case (f_read_addr$D_OUT)
      7'h04,
      7'h05,
      7'h06,
      7'h07,
      7'h08,
      7'h09,
      7'h0A,
      7'h0B,
      7'h0C,
      7'h0D,
      7'h0F,
      7'h16,
      7'h17,
      7'h18,
      7'h20:
	  dmi_read_data = dm_abstract_commands$av_read;
      7'h10,
      7'h11,
      7'h12,
      7'h13,
      7'h14,
      7'h15,
      7'h19,
      7'h30,
      7'h40,
      7'h5F,
      7'h60:
	  dmi_read_data = dm_run_control$av_read;
      7'h38, 7'h39, 7'h3A, 7'h3B, 7'h3C, 7'h3D, 7'h3E, 7'h3F:
	  dmi_read_data = dm_system_bus$av_read;
      default: dmi_read_data = 32'd0;
    endcase
  end
  assign RDY_dmi_read_data =
	     f_read_addr$EMPTY_N &&
	     (f_read_addr$D_OUT != 7'h38 && f_read_addr$D_OUT != 7'h39 &&
	      f_read_addr$D_OUT != 7'h3A &&
	      f_read_addr$D_OUT != 7'h3B &&
	      f_read_addr$D_OUT != 7'h3C &&
	      f_read_addr$D_OUT != 7'h3D &&
	      f_read_addr$D_OUT != 7'h3E &&
	      f_read_addr$D_OUT != 7'h3F ||
	      dm_system_bus$RDY_av_read) ;
  assign CAN_FIRE_dmi_read_data = RDY_dmi_read_data ;
  assign WILL_FIRE_dmi_read_data = EN_dmi_read_data ;

  // action method dmi_write
  assign RDY_dmi_write =
	     dm_run_control$dmactive && dm_run_control$RDY_write &&
	     dm_system_bus$RDY_write ;
  assign CAN_FIRE_dmi_write =
	     dm_run_control$dmactive && dm_run_control$RDY_write &&
	     dm_system_bus$RDY_write ;
  assign WILL_FIRE_dmi_write = EN_dmi_write ;

  // actionvalue method hart0_reset_client_request_get
  assign hart0_reset_client_request_get =
	     dm_run_control$hart0_reset_client_request_get ;
  assign RDY_hart0_reset_client_request_get =
	     dm_run_control$RDY_hart0_reset_client_request_get ;
  assign CAN_FIRE_hart0_reset_client_request_get =
	     dm_run_control$RDY_hart0_reset_client_request_get ;
  assign WILL_FIRE_hart0_reset_client_request_get =
	     EN_hart0_reset_client_request_get ;

  // action method hart0_reset_client_response_put
  assign RDY_hart0_reset_client_response_put =
	     dm_run_control$RDY_hart0_reset_client_response_put ;
  assign CAN_FIRE_hart0_reset_client_response_put =
	     dm_run_control$RDY_hart0_reset_client_response_put ;
  assign WILL_FIRE_hart0_reset_client_response_put =
	     EN_hart0_reset_client_response_put ;

  // actionvalue method hart0_client_run_halt_request_get
  assign hart0_client_run_halt_request_get =
	     dm_run_control$hart0_client_run_halt_request_get ;
  assign RDY_hart0_client_run_halt_request_get =
	     dm_run_control$RDY_hart0_client_run_halt_request_get ;
  assign CAN_FIRE_hart0_client_run_halt_request_get =
	     dm_run_control$RDY_hart0_client_run_halt_request_get ;
  assign WILL_FIRE_hart0_client_run_halt_request_get =
	     EN_hart0_client_run_halt_request_get ;

  // action method hart0_client_run_halt_response_put
  assign RDY_hart0_client_run_halt_response_put =
	     dm_run_control$RDY_hart0_client_run_halt_response_put ;
  assign CAN_FIRE_hart0_client_run_halt_response_put =
	     dm_run_control$RDY_hart0_client_run_halt_response_put ;
  assign WILL_FIRE_hart0_client_run_halt_response_put =
	     EN_hart0_client_run_halt_response_put ;

  // actionvalue method hart0_get_other_req_get
  assign hart0_get_other_req_get = dm_run_control$hart0_get_other_req_get ;
  assign RDY_hart0_get_other_req_get =
	     dm_run_control$RDY_hart0_get_other_req_get ;
  assign CAN_FIRE_hart0_get_other_req_get =
	     dm_run_control$RDY_hart0_get_other_req_get ;
  assign WILL_FIRE_hart0_get_other_req_get = EN_hart0_get_other_req_get ;

  // actionvalue method hart0_gpr_mem_client_request_get
  assign hart0_gpr_mem_client_request_get =
	     dm_abstract_commands$hart0_gpr_mem_client_request_get ;
  assign RDY_hart0_gpr_mem_client_request_get =
	     dm_abstract_commands$RDY_hart0_gpr_mem_client_request_get ;
  assign CAN_FIRE_hart0_gpr_mem_client_request_get =
	     dm_abstract_commands$RDY_hart0_gpr_mem_client_request_get ;
  assign WILL_FIRE_hart0_gpr_mem_client_request_get =
	     EN_hart0_gpr_mem_client_request_get ;

  // action method hart0_gpr_mem_client_response_put
  assign RDY_hart0_gpr_mem_client_response_put =
	     dm_abstract_commands$RDY_hart0_gpr_mem_client_response_put ;
  assign CAN_FIRE_hart0_gpr_mem_client_response_put =
	     dm_abstract_commands$RDY_hart0_gpr_mem_client_response_put ;
  assign WILL_FIRE_hart0_gpr_mem_client_response_put =
	     EN_hart0_gpr_mem_client_response_put ;

  // actionvalue method hart0_csr_mem_client_request_get
  assign hart0_csr_mem_client_request_get =
	     dm_abstract_commands$hart0_csr_mem_client_request_get ;
  assign RDY_hart0_csr_mem_client_request_get =
	     dm_abstract_commands$RDY_hart0_csr_mem_client_request_get ;
  assign CAN_FIRE_hart0_csr_mem_client_request_get =
	     dm_abstract_commands$RDY_hart0_csr_mem_client_request_get ;
  assign WILL_FIRE_hart0_csr_mem_client_request_get =
	     EN_hart0_csr_mem_client_request_get ;

  // action method hart0_csr_mem_client_response_put
  assign RDY_hart0_csr_mem_client_response_put =
	     dm_abstract_commands$RDY_hart0_csr_mem_client_response_put ;
  assign CAN_FIRE_hart0_csr_mem_client_response_put =
	     dm_abstract_commands$RDY_hart0_csr_mem_client_response_put ;
  assign WILL_FIRE_hart0_csr_mem_client_response_put =
	     EN_hart0_csr_mem_client_response_put ;

  // actionvalue method ndm_reset_client_request_get
  assign ndm_reset_client_request_get =
	     dm_run_control$ndm_reset_client_request_get ;
  assign RDY_ndm_reset_client_request_get =
	     dm_run_control$RDY_ndm_reset_client_request_get ;
  assign CAN_FIRE_ndm_reset_client_request_get =
	     dm_run_control$RDY_ndm_reset_client_request_get ;
  assign WILL_FIRE_ndm_reset_client_request_get =
	     EN_ndm_reset_client_request_get ;

  // action method ndm_reset_client_response_put
  assign RDY_ndm_reset_client_response_put =
	     dm_run_control$RDY_ndm_reset_client_response_put ;
  assign CAN_FIRE_ndm_reset_client_response_put =
	     dm_run_control$RDY_ndm_reset_client_response_put ;
  assign WILL_FIRE_ndm_reset_client_response_put =
	     EN_ndm_reset_client_response_put ;

  // value method master_aw_canPeek
  assign master_aw_canPeek = dm_system_bus$master_aw_canPeek ;

  // value method master_aw_peek
  assign master_aw_peek = dm_system_bus$master_aw_peek ;
  assign RDY_master_aw_peek = dm_system_bus$RDY_master_aw_peek ;

  // action method master_aw_drop
  assign RDY_master_aw_drop = dm_system_bus$RDY_master_aw_drop ;
  assign CAN_FIRE_master_aw_drop = dm_system_bus$RDY_master_aw_drop ;
  assign WILL_FIRE_master_aw_drop = EN_master_aw_drop ;

  // value method master_w_canPeek
  assign master_w_canPeek = dm_system_bus$master_w_canPeek ;

  // value method master_w_peek
  assign master_w_peek = dm_system_bus$master_w_peek ;
  assign RDY_master_w_peek = dm_system_bus$RDY_master_w_peek ;

  // action method master_w_drop
  assign RDY_master_w_drop = dm_system_bus$RDY_master_w_drop ;
  assign CAN_FIRE_master_w_drop = dm_system_bus$RDY_master_w_drop ;
  assign WILL_FIRE_master_w_drop = EN_master_w_drop ;

  // value method master_b_canPut
  assign master_b_canPut = dm_system_bus$master_b_canPut ;

  // action method master_b_put
  assign RDY_master_b_put = dm_system_bus$RDY_master_b_put ;
  assign CAN_FIRE_master_b_put = dm_system_bus$RDY_master_b_put ;
  assign WILL_FIRE_master_b_put = EN_master_b_put ;

  // value method master_ar_canPeek
  assign master_ar_canPeek = dm_system_bus$master_ar_canPeek ;

  // value method master_ar_peek
  assign master_ar_peek = dm_system_bus$master_ar_peek ;
  assign RDY_master_ar_peek = dm_system_bus$RDY_master_ar_peek ;

  // action method master_ar_drop
  assign RDY_master_ar_drop = dm_system_bus$RDY_master_ar_drop ;
  assign CAN_FIRE_master_ar_drop = dm_system_bus$RDY_master_ar_drop ;
  assign WILL_FIRE_master_ar_drop = EN_master_ar_drop ;

  // value method master_r_canPut
  assign master_r_canPut = dm_system_bus$master_r_canPut ;

  // action method master_r_put
  assign RDY_master_r_put = dm_system_bus$RDY_master_r_put ;
  assign CAN_FIRE_master_r_put = dm_system_bus$RDY_master_r_put ;
  assign WILL_FIRE_master_r_put = EN_master_r_put ;

  // submodule dm_abstract_commands
  mkDM_Abstract_Commands dm_abstract_commands(.CLK(CLK),
					      .RST_N(RST_N),
					      .av_read_dm_addr(dm_abstract_commands$av_read_dm_addr),
					      .hart0_csr_mem_client_response_put(dm_abstract_commands$hart0_csr_mem_client_response_put),
					      .hart0_gpr_mem_client_response_put(dm_abstract_commands$hart0_gpr_mem_client_response_put),
					      .write_dm_addr(dm_abstract_commands$write_dm_addr),
					      .write_dm_word(dm_abstract_commands$write_dm_word),
					      .EN_reset(dm_abstract_commands$EN_reset),
					      .EN_av_read(dm_abstract_commands$EN_av_read),
					      .EN_write(dm_abstract_commands$EN_write),
					      .EN_hart0_gpr_mem_client_request_get(dm_abstract_commands$EN_hart0_gpr_mem_client_request_get),
					      .EN_hart0_gpr_mem_client_response_put(dm_abstract_commands$EN_hart0_gpr_mem_client_response_put),
					      .EN_hart0_csr_mem_client_request_get(dm_abstract_commands$EN_hart0_csr_mem_client_request_get),
					      .EN_hart0_csr_mem_client_response_put(dm_abstract_commands$EN_hart0_csr_mem_client_response_put),
					      .RDY_reset(),
					      .av_read(dm_abstract_commands$av_read),
					      .RDY_av_read(),
					      .RDY_write(),
					      .hart0_gpr_mem_client_request_get(dm_abstract_commands$hart0_gpr_mem_client_request_get),
					      .RDY_hart0_gpr_mem_client_request_get(dm_abstract_commands$RDY_hart0_gpr_mem_client_request_get),
					      .RDY_hart0_gpr_mem_client_response_put(dm_abstract_commands$RDY_hart0_gpr_mem_client_response_put),
					      .hart0_csr_mem_client_request_get(dm_abstract_commands$hart0_csr_mem_client_request_get),
					      .RDY_hart0_csr_mem_client_request_get(dm_abstract_commands$RDY_hart0_csr_mem_client_request_get),
					      .RDY_hart0_csr_mem_client_response_put(dm_abstract_commands$RDY_hart0_csr_mem_client_response_put));

  // submodule dm_run_control
  mkDM_Run_Control dm_run_control(.CLK(CLK),
				  .RST_N(RST_N),
				  .av_read_dm_addr(dm_run_control$av_read_dm_addr),
				  .hart0_client_run_halt_response_put(dm_run_control$hart0_client_run_halt_response_put),
				  .hart0_reset_client_response_put(dm_run_control$hart0_reset_client_response_put),
				  .ndm_reset_client_response_put(dm_run_control$ndm_reset_client_response_put),
				  .write_dm_addr(dm_run_control$write_dm_addr),
				  .write_dm_word(dm_run_control$write_dm_word),
				  .EN_reset(dm_run_control$EN_reset),
				  .EN_av_read(dm_run_control$EN_av_read),
				  .EN_write(dm_run_control$EN_write),
				  .EN_hart0_reset_client_request_get(dm_run_control$EN_hart0_reset_client_request_get),
				  .EN_hart0_reset_client_response_put(dm_run_control$EN_hart0_reset_client_response_put),
				  .EN_hart0_client_run_halt_request_get(dm_run_control$EN_hart0_client_run_halt_request_get),
				  .EN_hart0_client_run_halt_response_put(dm_run_control$EN_hart0_client_run_halt_response_put),
				  .EN_hart0_get_other_req_get(dm_run_control$EN_hart0_get_other_req_get),
				  .EN_ndm_reset_client_request_get(dm_run_control$EN_ndm_reset_client_request_get),
				  .EN_ndm_reset_client_response_put(dm_run_control$EN_ndm_reset_client_response_put),
				  .dmactive(dm_run_control$dmactive),
				  .RDY_dmactive(),
				  .RDY_reset(),
				  .av_read(dm_run_control$av_read),
				  .RDY_av_read(),
				  .RDY_write(dm_run_control$RDY_write),
				  .hart0_reset_client_request_get(dm_run_control$hart0_reset_client_request_get),
				  .RDY_hart0_reset_client_request_get(dm_run_control$RDY_hart0_reset_client_request_get),
				  .RDY_hart0_reset_client_response_put(dm_run_control$RDY_hart0_reset_client_response_put),
				  .hart0_client_run_halt_request_get(dm_run_control$hart0_client_run_halt_request_get),
				  .RDY_hart0_client_run_halt_request_get(dm_run_control$RDY_hart0_client_run_halt_request_get),
				  .RDY_hart0_client_run_halt_response_put(dm_run_control$RDY_hart0_client_run_halt_response_put),
				  .hart0_get_other_req_get(dm_run_control$hart0_get_other_req_get),
				  .RDY_hart0_get_other_req_get(dm_run_control$RDY_hart0_get_other_req_get),
				  .ndm_reset_client_request_get(dm_run_control$ndm_reset_client_request_get),
				  .RDY_ndm_reset_client_request_get(dm_run_control$RDY_ndm_reset_client_request_get),
				  .RDY_ndm_reset_client_response_put(dm_run_control$RDY_ndm_reset_client_response_put));

  // submodule dm_system_bus
  mkDM_System_Bus dm_system_bus(.CLK(CLK),
				.RST_N(RST_N),
				.av_read_dm_addr(dm_system_bus$av_read_dm_addr),
				.master_b_put_val(dm_system_bus$master_b_put_val),
				.master_r_put_val(dm_system_bus$master_r_put_val),
				.write_dm_addr(dm_system_bus$write_dm_addr),
				.write_dm_word(dm_system_bus$write_dm_word),
				.EN_reset(dm_system_bus$EN_reset),
				.EN_av_read(dm_system_bus$EN_av_read),
				.EN_write(dm_system_bus$EN_write),
				.EN_master_aw_drop(dm_system_bus$EN_master_aw_drop),
				.EN_master_w_drop(dm_system_bus$EN_master_w_drop),
				.EN_master_b_put(dm_system_bus$EN_master_b_put),
				.EN_master_ar_drop(dm_system_bus$EN_master_ar_drop),
				.EN_master_r_put(dm_system_bus$EN_master_r_put),
				.RDY_reset(),
				.av_read(dm_system_bus$av_read),
				.RDY_av_read(dm_system_bus$RDY_av_read),
				.RDY_write(dm_system_bus$RDY_write),
				.master_aw_canPeek(dm_system_bus$master_aw_canPeek),
				.master_aw_peek(dm_system_bus$master_aw_peek),
				.RDY_master_aw_peek(dm_system_bus$RDY_master_aw_peek),
				.RDY_master_aw_drop(dm_system_bus$RDY_master_aw_drop),
				.master_w_canPeek(dm_system_bus$master_w_canPeek),
				.master_w_peek(dm_system_bus$master_w_peek),
				.RDY_master_w_peek(dm_system_bus$RDY_master_w_peek),
				.RDY_master_w_drop(dm_system_bus$RDY_master_w_drop),
				.master_b_canPut(dm_system_bus$master_b_canPut),
				.RDY_master_b_put(dm_system_bus$RDY_master_b_put),
				.master_ar_canPeek(dm_system_bus$master_ar_canPeek),
				.master_ar_peek(dm_system_bus$master_ar_peek),
				.RDY_master_ar_peek(dm_system_bus$RDY_master_ar_peek),
				.RDY_master_ar_drop(dm_system_bus$RDY_master_ar_drop),
				.master_r_canPut(dm_system_bus$master_r_canPut),
				.RDY_master_r_put(dm_system_bus$RDY_master_r_put));

  // submodule f_read_addr
  FIFO1 #(.width(32'd7), .guarded(1'd1)) f_read_addr(.RST(RST_N),
						     .CLK(CLK),
						     .D_IN(f_read_addr$D_IN),
						     .ENQ(f_read_addr$ENQ),
						     .DEQ(f_read_addr$DEQ),
						     .CLR(f_read_addr$CLR),
						     .D_OUT(f_read_addr$D_OUT),
						     .FULL_N(f_read_addr$FULL_N),
						     .EMPTY_N(f_read_addr$EMPTY_N));

  // rule RL_rl_reset
  assign CAN_FIRE_RL_rl_reset = !dm_run_control$dmactive ;
  assign WILL_FIRE_RL_rl_reset = CAN_FIRE_RL_rl_reset ;

  // submodule dm_abstract_commands
  assign dm_abstract_commands$av_read_dm_addr = f_read_addr$D_OUT ;
  assign dm_abstract_commands$hart0_csr_mem_client_response_put =
	     hart0_csr_mem_client_response_put ;
  assign dm_abstract_commands$hart0_gpr_mem_client_response_put =
	     hart0_gpr_mem_client_response_put ;
  assign dm_abstract_commands$write_dm_addr = dmi_write_dm_addr ;
  assign dm_abstract_commands$write_dm_word = dmi_write_dm_word ;
  assign dm_abstract_commands$EN_reset = CAN_FIRE_RL_rl_reset ;
  assign dm_abstract_commands$EN_av_read =
	     EN_dmi_read_data &&
	     (f_read_addr$D_OUT == 7'h16 || f_read_addr$D_OUT == 7'h17 ||
	      f_read_addr$D_OUT == 7'h04 ||
	      f_read_addr$D_OUT == 7'h05 ||
	      f_read_addr$D_OUT == 7'h06 ||
	      f_read_addr$D_OUT == 7'h07 ||
	      f_read_addr$D_OUT == 7'h08 ||
	      f_read_addr$D_OUT == 7'h09 ||
	      f_read_addr$D_OUT == 7'h0A ||
	      f_read_addr$D_OUT == 7'h0B ||
	      f_read_addr$D_OUT == 7'h0C ||
	      f_read_addr$D_OUT == 7'h0D ||
	      f_read_addr$D_OUT == 7'h0F ||
	      f_read_addr$D_OUT == 7'h18 ||
	      f_read_addr$D_OUT == 7'h20) ;
  assign dm_abstract_commands$EN_write =
	     EN_dmi_write &&
	     (dmi_write_dm_addr == 7'h16 || dmi_write_dm_addr == 7'h17 ||
	      dmi_write_dm_addr == 7'h04 ||
	      dmi_write_dm_addr == 7'h05 ||
	      dmi_write_dm_addr == 7'h06 ||
	      dmi_write_dm_addr == 7'h07 ||
	      dmi_write_dm_addr == 7'h08 ||
	      dmi_write_dm_addr == 7'h09 ||
	      dmi_write_dm_addr == 7'h0A ||
	      dmi_write_dm_addr == 7'h0B ||
	      dmi_write_dm_addr == 7'h0C ||
	      dmi_write_dm_addr == 7'h0D ||
	      dmi_write_dm_addr == 7'h0F ||
	      dmi_write_dm_addr == 7'h18 ||
	      dmi_write_dm_addr == 7'h20) ;
  assign dm_abstract_commands$EN_hart0_gpr_mem_client_request_get =
	     EN_hart0_gpr_mem_client_request_get ;
  assign dm_abstract_commands$EN_hart0_gpr_mem_client_response_put =
	     EN_hart0_gpr_mem_client_response_put ;
  assign dm_abstract_commands$EN_hart0_csr_mem_client_request_get =
	     EN_hart0_csr_mem_client_request_get ;
  assign dm_abstract_commands$EN_hart0_csr_mem_client_response_put =
	     EN_hart0_csr_mem_client_response_put ;

  // submodule dm_run_control
  assign dm_run_control$av_read_dm_addr = f_read_addr$D_OUT ;
  assign dm_run_control$hart0_client_run_halt_response_put =
	     hart0_client_run_halt_response_put ;
  assign dm_run_control$hart0_reset_client_response_put =
	     hart0_reset_client_response_put ;
  assign dm_run_control$ndm_reset_client_response_put =
	     ndm_reset_client_response_put ;
  assign dm_run_control$write_dm_addr = dmi_write_dm_addr ;
  assign dm_run_control$write_dm_word = dmi_write_dm_word ;
  assign dm_run_control$EN_reset = CAN_FIRE_RL_rl_reset ;
  assign dm_run_control$EN_av_read =
	     EN_dmi_read_data &&
	     (f_read_addr$D_OUT == 7'h10 || f_read_addr$D_OUT == 7'h11 ||
	      f_read_addr$D_OUT == 7'h12 ||
	      f_read_addr$D_OUT == 7'h13 ||
	      f_read_addr$D_OUT == 7'h14 ||
	      f_read_addr$D_OUT == 7'h15 ||
	      f_read_addr$D_OUT == 7'h19 ||
	      f_read_addr$D_OUT == 7'h30 ||
	      f_read_addr$D_OUT == 7'h40 ||
	      f_read_addr$D_OUT == 7'h5F ||
	      f_read_addr$D_OUT == 7'h60) ;
  assign dm_run_control$EN_write =
	     EN_dmi_write &&
	     (dmi_write_dm_addr == 7'h10 || dmi_write_dm_addr == 7'h11 ||
	      dmi_write_dm_addr == 7'h12 ||
	      dmi_write_dm_addr == 7'h13 ||
	      dmi_write_dm_addr == 7'h14 ||
	      dmi_write_dm_addr == 7'h15 ||
	      dmi_write_dm_addr == 7'h19 ||
	      dmi_write_dm_addr == 7'h30 ||
	      dmi_write_dm_addr == 7'h40 ||
	      dmi_write_dm_addr == 7'h5F ||
	      dmi_write_dm_addr == 7'h60) ;
  assign dm_run_control$EN_hart0_reset_client_request_get =
	     EN_hart0_reset_client_request_get ;
  assign dm_run_control$EN_hart0_reset_client_response_put =
	     EN_hart0_reset_client_response_put ;
  assign dm_run_control$EN_hart0_client_run_halt_request_get =
	     EN_hart0_client_run_halt_request_get ;
  assign dm_run_control$EN_hart0_client_run_halt_response_put =
	     EN_hart0_client_run_halt_response_put ;
  assign dm_run_control$EN_hart0_get_other_req_get =
	     EN_hart0_get_other_req_get ;
  assign dm_run_control$EN_ndm_reset_client_request_get =
	     EN_ndm_reset_client_request_get ;
  assign dm_run_control$EN_ndm_reset_client_response_put =
	     EN_ndm_reset_client_response_put ;

  // submodule dm_system_bus
  assign dm_system_bus$av_read_dm_addr = f_read_addr$D_OUT ;
  assign dm_system_bus$master_b_put_val = master_b_put_val ;
  assign dm_system_bus$master_r_put_val = master_r_put_val ;
  assign dm_system_bus$write_dm_addr = dmi_write_dm_addr ;
  assign dm_system_bus$write_dm_word = dmi_write_dm_word ;
  assign dm_system_bus$EN_reset = CAN_FIRE_RL_rl_reset ;
  assign dm_system_bus$EN_av_read =
	     EN_dmi_read_data &&
	     (f_read_addr$D_OUT == 7'h38 || f_read_addr$D_OUT == 7'h39 ||
	      f_read_addr$D_OUT == 7'h3A ||
	      f_read_addr$D_OUT == 7'h3B ||
	      f_read_addr$D_OUT == 7'h3C ||
	      f_read_addr$D_OUT == 7'h3D ||
	      f_read_addr$D_OUT == 7'h3E ||
	      f_read_addr$D_OUT == 7'h3F) ;
  assign dm_system_bus$EN_write =
	     EN_dmi_write &&
	     (dmi_write_dm_addr == 7'h38 || dmi_write_dm_addr == 7'h39 ||
	      dmi_write_dm_addr == 7'h3A ||
	      dmi_write_dm_addr == 7'h3B ||
	      dmi_write_dm_addr == 7'h3C ||
	      dmi_write_dm_addr == 7'h3D ||
	      dmi_write_dm_addr == 7'h3E ||
	      dmi_write_dm_addr == 7'h3F) ;
  assign dm_system_bus$EN_master_aw_drop = EN_master_aw_drop ;
  assign dm_system_bus$EN_master_w_drop = EN_master_w_drop ;
  assign dm_system_bus$EN_master_b_put = EN_master_b_put ;
  assign dm_system_bus$EN_master_ar_drop = EN_master_ar_drop ;
  assign dm_system_bus$EN_master_r_put = EN_master_r_put ;

  // submodule f_read_addr
  assign f_read_addr$D_IN = dmi_read_addr_dm_addr ;
  assign f_read_addr$ENQ = EN_dmi_read_addr ;
  assign f_read_addr$DEQ = EN_dmi_read_data ;
  assign f_read_addr$CLR = 1'b0 ;

  // handling of system tasks

  // synopsys translate_off
  always@(negedge CLK)
  begin
    #0;
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_rl_reset)
	begin
	  v__h485 = $stime;
	  #0;
	end
    v__h479 = v__h485 / 32'd10;
    if (RST_N != `BSV_RESET_VALUE)
      if (WILL_FIRE_RL_rl_reset) $display("%0d: Debug_Module reset", v__h479);
  end
  // synopsys translate_on
endmodule  // mkDebug_Module

