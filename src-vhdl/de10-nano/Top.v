module Top(

      ///////// FPGA /////////
      input              FPGA_CLK1_50,

      ///////// HDMI /////////
      inout              HDMI_I2C_SCL,
      inout              HDMI_I2C_SDA,
      inout              HDMI_I2S,
      inout              HDMI_LRCLK,
      inout              HDMI_MCLK,
      inout              HDMI_SCLK,
      output             HDMI_TX_CLK,
      output      [23:0] HDMI_TX_D,
      output             HDMI_TX_DE,
      output             HDMI_TX_HS,
      input              HDMI_TX_INT,
      output             HDMI_TX_VS
);



wire				reset_n;
//Video Pattern Generator
wire	[3:0]		vpg_disp_mode;
wire				vpg_pclk;
wire				vpg_de, vpg_hs, vpg_vs;
wire	[23:0]	vpg_data;

I2C_HDMI_Config u_I2C_HDMI_Config (
	.iCLK(FPGA_CLK1_50),
	.iRST_N(reset_n),
	.I2C_SCLK(HDMI_I2C_SCL),
	.I2C_SDAT(HDMI_I2C_SDA),
	.HDMI_TX_INT(HDMI_TX_INT)
	 );

assign reset_n = 1'b1;

wire clk_25;
wire gen_clk_locked;

video u_video (
	.refclk_50_clk(FPGA_CLK1_50),
	.rst_reset(!reset_n),
	.outclk_25_clk(clk_25),
	.locked_export(gen_clk_locked));

assign HDMI_TX_CLK = clk_25;
	
vpg u_vpg (
	.clk_25(clk_25),
	.reset_n(gen_clk_locked),
	.vpg_de(HDMI_TX_DE),
	.vpg_hs(HDMI_TX_HS),
	.vpg_vs(HDMI_TX_VS),
	.vpg_r(HDMI_TX_D[23:16]),
	.vpg_g(HDMI_TX_D[15:8]),
	.vpg_b(HDMI_TX_D[7:0]) );

endmodule
