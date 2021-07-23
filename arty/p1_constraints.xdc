# this is only in arty_cheri from valentin
#set_property MARK_DEBUG false [get_nets system_i/ssith_processor_0/inst/jtagtap/CLK_jtag_tclk_out]

#set_property MARK_DEBUG true [get_nets jtagtap/CLK_jtag_tclk_out]
#create_clock -name tck_internal -period 40.000 -waveform {0.000 20.000} [get_nets jtagtap/CLK_jtag_tclk_out]
#set_clock_uncertainty 2.00 [get_clocks *tck_internal*]

#set_property C_CLK_INPUT_FREQ_HZ 300000000 [get_debug_cores dbg_hub]
#set_property C_ENABLE_CLK_DIVIDER false [get_debug_cores dbg_hub]
#set_property C_USER_SCAN_CHAIN 1 [get_debug_cores dbg_hub]
#connect_debug_port dbg_hub/clk [get_nets clk]

#set_clock_groups -asynchronous -group tck_internal -group {default_250mhz_clk1_clk_p mmcm_clkout0 mmcm_clkout1 tck}

set_property BITSTREAM.GENERAL.COMPRESS TRUE [current_design]
#set_property BITSTREAM.CONFIG.CONFIGRATE 3 [current_design]
