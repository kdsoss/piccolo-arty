
module xilinx_jtag(
		   input  clk,

		   output tck,
		   output tdi,
		   output tms,
		   input  tdo
		   );

   wire 		  tck_internal;

   BUFG tck_buf(
		.I (tck_internal),
		.O (tck)
		);

   BSCANE2 #(
	     .DISABLE_JTAG("FALSE"),
	     .JTAG_CHAIN(3)
	     )
   bscane2_user3(
		 .DRCK (),
		 .RESET (),
		 .RUNTEST (),
		 .CAPTURE (),
		 .SEL (),
		 .SHIFT (),
		 .TCK (tck_internal),
		 .TDI (tdi),
		 .TMS (tms),
		 .UPDATE (),
		 .TDO (tdo)
		 );

   BSCANE2 #(
	     .DISABLE_JTAG("FALSE"),
	     .JTAG_CHAIN(2)
	     )
   bscane2_user2(
		 .DRCK (),
		 .RESET (),
		 .RUNTEST (),
		 .CAPTURE (),
		 .SEL (),
		 .SHIFT (),
		 .TCK (),
		 .TDI (),
		 .TMS (),
		 .UPDATE (),
		 .TDO (tdo)
		 );

endmodule
