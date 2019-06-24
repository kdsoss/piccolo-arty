//
// Generated by Bluespec Compiler, version 2018.10.beta1 (build e1df8052c, 2018-10-17)
//
// On Mon Jun 24 12:21:13 BST 2019
//
//
// Ports:
// Name                         I/O  size props
// client_request_get             O    38 reg
// RDY_client_request_get         O     1 reg
// RDY_client_response_put        O     1 reg
// RDY_server_request_put         O     1 reg
// server_response_get            O    33 reg
// RDY_server_response_get        O     1 reg
// trace_data_out_get             O   234 reg
// RDY_trace_data_out_get         O     1 reg
// CLK                            I     1 clock
// RST_N                          I     1 reset
// client_response_put            I    33 reg
// server_request_put             I    38 reg
// EN_client_response_put         I     1
// EN_server_request_put          I     1
// EN_client_request_get          I     1
// EN_server_response_get         I     1
// EN_trace_data_out_get          I     1
//
// No combinational paths from inputs to outputs
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

module mkDM_GPR_Tap(CLK,
		    RST_N,

		    EN_client_request_get,
		    client_request_get,
		    RDY_client_request_get,

		    client_response_put,
		    EN_client_response_put,
		    RDY_client_response_put,

		    server_request_put,
		    EN_server_request_put,
		    RDY_server_request_put,

		    EN_server_response_get,
		    server_response_get,
		    RDY_server_response_get,

		    EN_trace_data_out_get,
		    trace_data_out_get,
		    RDY_trace_data_out_get);
  input  CLK;
  input  RST_N;

  // actionvalue method client_request_get
  input  EN_client_request_get;
  output [37 : 0] client_request_get;
  output RDY_client_request_get;

  // action method client_response_put
  input  [32 : 0] client_response_put;
  input  EN_client_response_put;
  output RDY_client_response_put;

  // action method server_request_put
  input  [37 : 0] server_request_put;
  input  EN_server_request_put;
  output RDY_server_request_put;

  // actionvalue method server_response_get
  input  EN_server_response_get;
  output [32 : 0] server_response_get;
  output RDY_server_response_get;

  // actionvalue method trace_data_out_get
  input  EN_trace_data_out_get;
  output [233 : 0] trace_data_out_get;
  output RDY_trace_data_out_get;

  // signals for module outputs
  wire [233 : 0] trace_data_out_get;
  wire [37 : 0] client_request_get;
  wire [32 : 0] server_response_get;
  wire RDY_client_request_get,
       RDY_client_response_put,
       RDY_server_request_put,
       RDY_server_response_get,
       RDY_trace_data_out_get;

  // ports of submodule f_req_in
  wire [37 : 0] f_req_in$D_IN, f_req_in$D_OUT;
  wire f_req_in$CLR,
       f_req_in$DEQ,
       f_req_in$EMPTY_N,
       f_req_in$ENQ,
       f_req_in$FULL_N;

  // ports of submodule f_req_out
  wire [37 : 0] f_req_out$D_IN, f_req_out$D_OUT;
  wire f_req_out$CLR,
       f_req_out$DEQ,
       f_req_out$EMPTY_N,
       f_req_out$ENQ,
       f_req_out$FULL_N;

  // ports of submodule f_rsp
  wire [32 : 0] f_rsp$D_IN, f_rsp$D_OUT;
  wire f_rsp$CLR, f_rsp$DEQ, f_rsp$EMPTY_N, f_rsp$ENQ, f_rsp$FULL_N;

  // ports of submodule f_trace_data
  wire [233 : 0] f_trace_data$D_IN, f_trace_data$D_OUT;
  wire f_trace_data$CLR,
       f_trace_data$DEQ,
       f_trace_data$EMPTY_N,
       f_trace_data$ENQ,
       f_trace_data$FULL_N;

  // rule scheduling signals
  wire CAN_FIRE_RL_request,
       CAN_FIRE_client_request_get,
       CAN_FIRE_client_response_put,
       CAN_FIRE_server_request_put,
       CAN_FIRE_server_response_get,
       CAN_FIRE_trace_data_out_get,
       WILL_FIRE_RL_request,
       WILL_FIRE_client_request_get,
       WILL_FIRE_client_response_put,
       WILL_FIRE_server_request_put,
       WILL_FIRE_server_response_get,
       WILL_FIRE_trace_data_out_get;

  // actionvalue method client_request_get
  assign client_request_get = f_req_out$D_OUT ;
  assign RDY_client_request_get = f_req_out$EMPTY_N ;
  assign CAN_FIRE_client_request_get = f_req_out$EMPTY_N ;
  assign WILL_FIRE_client_request_get = EN_client_request_get ;

  // action method client_response_put
  assign RDY_client_response_put = f_rsp$FULL_N ;
  assign CAN_FIRE_client_response_put = f_rsp$FULL_N ;
  assign WILL_FIRE_client_response_put = EN_client_response_put ;

  // action method server_request_put
  assign RDY_server_request_put = f_req_in$FULL_N ;
  assign CAN_FIRE_server_request_put = f_req_in$FULL_N ;
  assign WILL_FIRE_server_request_put = EN_server_request_put ;

  // actionvalue method server_response_get
  assign server_response_get = f_rsp$D_OUT ;
  assign RDY_server_response_get = f_rsp$EMPTY_N ;
  assign CAN_FIRE_server_response_get = f_rsp$EMPTY_N ;
  assign WILL_FIRE_server_response_get = EN_server_response_get ;

  // actionvalue method trace_data_out_get
  assign trace_data_out_get = f_trace_data$D_OUT ;
  assign RDY_trace_data_out_get = f_trace_data$EMPTY_N ;
  assign CAN_FIRE_trace_data_out_get = f_trace_data$EMPTY_N ;
  assign WILL_FIRE_trace_data_out_get = EN_trace_data_out_get ;

  // submodule f_req_in
  FIFO2 #(.width(32'd38), .guarded(32'd1)) f_req_in(.RST(RST_N),
						    .CLK(CLK),
						    .D_IN(f_req_in$D_IN),
						    .ENQ(f_req_in$ENQ),
						    .DEQ(f_req_in$DEQ),
						    .CLR(f_req_in$CLR),
						    .D_OUT(f_req_in$D_OUT),
						    .FULL_N(f_req_in$FULL_N),
						    .EMPTY_N(f_req_in$EMPTY_N));

  // submodule f_req_out
  FIFO2 #(.width(32'd38), .guarded(32'd1)) f_req_out(.RST(RST_N),
						     .CLK(CLK),
						     .D_IN(f_req_out$D_IN),
						     .ENQ(f_req_out$ENQ),
						     .DEQ(f_req_out$DEQ),
						     .CLR(f_req_out$CLR),
						     .D_OUT(f_req_out$D_OUT),
						     .FULL_N(f_req_out$FULL_N),
						     .EMPTY_N(f_req_out$EMPTY_N));

  // submodule f_rsp
  FIFO2 #(.width(32'd33), .guarded(32'd1)) f_rsp(.RST(RST_N),
						 .CLK(CLK),
						 .D_IN(f_rsp$D_IN),
						 .ENQ(f_rsp$ENQ),
						 .DEQ(f_rsp$DEQ),
						 .CLR(f_rsp$CLR),
						 .D_OUT(f_rsp$D_OUT),
						 .FULL_N(f_rsp$FULL_N),
						 .EMPTY_N(f_rsp$EMPTY_N));

  // submodule f_trace_data
  FIFO2 #(.width(32'd234), .guarded(32'd1)) f_trace_data(.RST(RST_N),
							 .CLK(CLK),
							 .D_IN(f_trace_data$D_IN),
							 .ENQ(f_trace_data$ENQ),
							 .DEQ(f_trace_data$DEQ),
							 .CLR(f_trace_data$CLR),
							 .D_OUT(f_trace_data$D_OUT),
							 .FULL_N(f_trace_data$FULL_N),
							 .EMPTY_N(f_trace_data$EMPTY_N));

  // rule RL_request
  assign CAN_FIRE_RL_request =
	     f_req_in$EMPTY_N && f_req_out$FULL_N &&
	     (!f_req_in$D_OUT[37] || f_trace_data$FULL_N) ;
  assign WILL_FIRE_RL_request = CAN_FIRE_RL_request ;

  // submodule f_req_in
  assign f_req_in$D_IN = server_request_put ;
  assign f_req_in$ENQ = EN_server_request_put ;
  assign f_req_in$DEQ = CAN_FIRE_RL_request ;
  assign f_req_in$CLR = 1'b0 ;

  // submodule f_req_out
  assign f_req_out$D_IN = f_req_in$D_OUT ;
  assign f_req_out$ENQ = CAN_FIRE_RL_request ;
  assign f_req_out$DEQ = EN_client_request_get ;
  assign f_req_out$CLR = 1'b0 ;

  // submodule f_rsp
  assign f_rsp$D_IN = client_response_put ;
  assign f_rsp$ENQ = EN_client_response_put ;
  assign f_rsp$DEQ = EN_server_response_get ;
  assign f_rsp$CLR = 1'b0 ;

  // submodule f_trace_data
  assign f_trace_data$D_IN =
	     { 69'h0355555554AAAAAAAA,
	       f_req_in$D_OUT[36:0],
	       128'hAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA } ;
  assign f_trace_data$ENQ = WILL_FIRE_RL_request && f_req_in$D_OUT[37] ;
  assign f_trace_data$DEQ = EN_trace_data_out_get ;
  assign f_trace_data$CLR = 1'b0 ;
endmodule  // mkDM_GPR_Tap

