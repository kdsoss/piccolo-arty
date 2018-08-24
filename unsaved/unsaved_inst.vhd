	component unsaved is
		port (
			clk_clk         : in  std_logic                    := 'X'; -- clk
			reset_reset_n   : in  std_logic                    := 'X'; -- reset_n
			leds_out_export : out std_logic_vector(9 downto 0)         -- export
		);
	end component unsaved;

	u0 : component unsaved
		port map (
			clk_clk         => CONNECTED_TO_clk_clk,         --      clk.clk
			reset_reset_n   => CONNECTED_TO_reset_reset_n,   --    reset.reset_n
			leds_out_export => CONNECTED_TO_leds_out_export  -- leds_out.export
		);

