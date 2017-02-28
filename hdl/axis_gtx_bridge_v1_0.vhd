--------------------------------------------------------------------------------
--                       Paul Scherrer Institute (PSI)
--------------------------------------------------------------------------------
-- Unit    : axis_gtx_bridge_v1_0.vhd
-- Author  : Goran Marinkovic, Section Diagnostic
-- Version : $Revision: 1.8 $
--------------------------------------------------------------------------------
-- Copyright© PSI, Section Diagnostic
--------------------------------------------------------------------------------
-- Comment : This is the axis interface for the GTXE2 component.
--------------------------------------------------------------------------------
-- Std. library (platform) -----------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Work library (application) --------------------------------------------------
library axis_gtx_bridge_v1_0_lib;
use axis_gtx_bridge_v1_0_lib.gtx_link_package.all;
use axis_gtx_bridge_v1_0_lib.gtx_ipif_package.all;

entity axis_gtx_bridge_v1_0 is
   generic
   (
      --------------------------------------------------------------------------
      -- Simulation attributes
      --------------------------------------------------------------------------
      SIM_RESET_SPEEDUP           : boolean := FALSE;                         -- Set to "TRUE" to speed up sim reset
      --------------------------------------------------------------------------
      -- GTX timing
      --------------------------------------------------------------------------
      CPU_CLK_Hz                  : integer := 125000000;                      -- Frequency of the stable clock in [Hz]
      REF_CLK_Hz                  : integer := 125000000;                      -- Frequency of the reference clock in [Hz]
      BAUD_RATE_Mbps              : integer := 2500;                           -- Baudrate in MBit/s
      --------------------------------------------------------------------------
      -- AXIS interface
      --------------------------------------------------------------------------
      -- AXIS slave bus interface
      C_S00_AXIS_TDATA_WIDTH      : integer := 32;
      -- AXIS master bus interface
      C_M00_AXIS_TDATA_WIDTH      : integer := 32
   );
   port
   (
      --------------------------------------------------------------------------
      -- Debug
      --------------------------------------------------------------------------
      debug_clk                    : out    std_logic;
      debug                        : out    std_logic_vector(255 downto  0);
      --------------------------------------------------------------------------
      -- System
      --------------------------------------------------------------------------
      aclk                        : in    std_logic;
      aresetn                     : in    std_logic;
      --------------------------------------------------------------------------
      -- GTX interface
      --------------------------------------------------------------------------
      i_clk_gtx                   : in    std_logic_vector( 1 downto  0);

      i_gtx_rx_p                  : in    std_logic;
      i_gtx_rx_n                  : in    std_logic;
      o_gtx_tx_p                  : out   std_logic;
      o_gtx_tx_n                  : out   std_logic;

      o_lock_plb                  : out   std_logic;
      o_lock_axi                  : out   std_logic;
      o_lock_comma                : out   std_logic;
      --------------------------------------------------------------------------
      -- AXIS interface
      --------------------------------------------------------------------------
      -- AXIS slave bus interface
      s00_axis_tready             : out   std_logic;
      s00_axis_tdata              : in    std_logic_vector(C_S00_AXIS_TDATA_WIDTH - 1 downto  0);
      s00_axis_tuser              : in    std_logic_vector((C_S00_AXIS_TDATA_WIDTH / 8) - 1 downto  0);
      s00_axis_tvalid             : in    std_logic;

      -- AXIS master bus interface
      m00_axis_tvalid             : out   std_logic;
      m00_axis_tdata              : out   std_logic_vector(C_M00_AXIS_TDATA_WIDTH - 1 downto  0);
      m00_axis_tuser              : out   std_logic_vector((C_M00_AXIS_TDATA_WIDTH / 8) - 1 downto  0);
      m00_axis_tready             : in    std_logic
   );
end axis_gtx_bridge_v1_0;

architecture arch_imp of axis_gtx_bridge_v1_0 is

   -----------------------------------------------------------------------------
   -- Parameter
   -----------------------------------------------------------------------------
-- Vivado IP Editor does not work properly with "real" numbers, hence this work-around is/was needed
   constant CPU_CLK_MHz           : real := real(CPU_CLK_Hz) / 1000000.0;     -- Frequency of the stable clock in [MHz]
   constant REF_CLK_MHz           : real := real(REF_CLK_Hz) / 1000000.0;     -- Frequency of the reference clock in [MHz]
   constant BAUD_RATE_Gbps        : real := real(BAUD_RATE_Mbps) / 1000.0;    -- Baudrate in GBit/s
--   constant CPU_CLK_MHz           : real := 125.0;     -- Frequency of the stable clock in [MHz]
--   constant REF_CLK_MHz           : real := 125.0;     -- Frequency of the reference clock in [MHz]
--   constant BAUD_RATE_Gbps        : real := 2.5;       -- Baudrate in GBit/s

   -----------------------------------------------------------------------------
   -- GTX Interface
   -----------------------------------------------------------------------------
   signal   gtx_ctrl_in           : gtx_ctrl_in_type;
   signal   gtx_in                : gtx_in_type;
   signal   gtx_out               : gtx_out_type;

   signal   comma_lock_axi        : std_logic := '0';
   signal   comma_lock_axi_s      : std_logic := '0';
   signal   comma_lock_plb        : std_logic := '0';
   signal   comma_lock            : std_logic := '0';

   signal   rx_xoff               : std_logic := '0';
   signal   tx_xoff               : std_logic := '0';

   -- AXIS slave bus interface
   signal   rready                : std_logic;
   signal   rdata                 : std_logic_vector(C_S00_AXIS_TDATA_WIDTH - 1 downto  0);
   signal   ruser                 : std_logic_vector((C_S00_AXIS_TDATA_WIDTH / 8) - 1 downto  0);
   signal   rvalid                : std_logic;

   -- AXIS master bus interface
   signal   tvalid                : std_logic;
   signal   tdata                 : std_logic_vector(C_M00_AXIS_TDATA_WIDTH - 1 downto  0);
   signal   tuser                 : std_logic_vector((C_M00_AXIS_TDATA_WIDTH / 8) - 1 downto  0);
   signal   tready                : std_logic;

   --ML84
   signal i_errcnt_rst : std_logic;
   signal rxdisperr_cnt, rxnotintable_cnt : unsigned(31 downto 0);
   signal pll0lock, pll1lock      : std_logic;
   signal rxfifo_debug : std_logic_vector(127 downto 0);

begin

   -----------------------------------------------------------------------------
   -- Debug
   -----------------------------------------------------------------------------

   debug_clk             <= '0';
   debug(31 downto  0) <= std_logic_vector(rxdisperr_cnt);
   debug(63 downto 32) <= std_logic_vector(rxnotintable_cnt);
   debug( 79 downto  64) <= gtx_out.rx.rxdata;
   debug( 81 downto  80) <= gtx_out.rx.rxcharisk;
   debug( 83 downto  82) <= gtx_out.rx.rxdisperr;
   debug( 85 downto  84) <= gtx_out.rx.rxnotintable;
   debug(101 downto  86) <= gtx_in.tx.txdata;
   debug(103 downto 102) <= gtx_in.tx.txcharisk;
   debug(           104) <= gtx_out.pll.cplllock;
--   debug(           105) <= pll1lock;
   debug(           106) <= gtx_out.rx.rxresetdone;
   debug(           107) <= gtx_out.tx.txresetdone;
   debug(           108) <= gtx_out.rx.rxbyteisaligned;
   debug(           109) <= gtx_out.rx.rxbyterealign;
   debug(           110) <= gtx_out.rx.rxcommadet;
   --
   debug(           126) <= gtx_out.rx.rxusrclk;
   debug(           127) <= gtx_out.tx.txusrclk;

   debug(159 downto 128) <= rxfifo_debug(95 downto 64); --rxfifo DI
   debug(163 downto 160) <= rxfifo_debug(36 downto 33); --rxfifo DIP
   debug(           164) <= rxfifo_debug(          32); --rxfifo WEN
   debug(           165) <= rxfifo_debug(          08); --rxfifo get_word_low
   debug(           166) <= rxfifo_debug(          00); --rxfifo rxerror     
   debug(168 downto 167) <= rxfifo_debug(04 downto 03); --rxfifo rxerror_v   

   

   --ML84
   i_errcnt_rst <= '0';

   STATS_P : process(gtx_out.rx.rxusrclk)
   begin
       if rising_edge(gtx_out.rx.rxusrclk) then
           if (i_errcnt_rst = '1') then
               rxdisperr_cnt <= (others => '0');
           elsif (gtx_out.rx.rxdisperr /= "00") then
               rxdisperr_cnt <= rxdisperr_cnt + 1;
           end if;

           if (i_errcnt_rst = '1') then
               rxnotintable_cnt <= (others => '0');
           elsif (gtx_out.rx.rxnotintable /= "00") then
               rxnotintable_cnt <= rxnotintable_cnt + 1;
           end if;

       end if;
   end process;


   -----------------------------------------------------------------------------
   -- System
   -----------------------------------------------------------------------------
   gtx_ctrl_in.clk                <= aclk;

   -----------------------------------------------------------------------------
   -- GTX reference clock
   -----------------------------------------------------------------------------
   gtx_ctrl_in.gtrefclk0          <= i_clk_gtx( 0);
   gtx_ctrl_in.gtrefclk1          <= i_clk_gtx( 1);

   -----------------------------------------------------------------------------
   -- GTX Rx settings
   -----------------------------------------------------------------------------
   gtx_in.rx.rxpd                 <= "00";

   -----------------------------------------------------------------------------
   -- GTX Tx settings
   -----------------------------------------------------------------------------
   gtx_in.tx.txpd                 <= "00";

   -----------------------------------------------------------------------------
   -- GTX instance
   -----------------------------------------------------------------------------
   gtx_gtxe2_inst: entity axis_gtx_bridge_v1_0_lib.gtx_link
   generic map
   (
      --------------------------------------------------------------------------
      -- Simulation attributes
      --------------------------------------------------------------------------
      SIM_RESET_SPEEDUP           => SIM_RESET_SPEEDUP,                       -- Set to "TRUE" to speed up sim reset
      --------------------------------------------------------------------------
      -- Startup timing
      --------------------------------------------------------------------------
      CPU_CLK_MHz                 => CPU_CLK_MHz,                             -- Frequency of the stable clock in [MHz]
      REF_CLK_MHz                 => REF_CLK_MHz,                             -- Frequency of the reference clock in [MHz]
      BAUD_RATE_Gbps              => BAUD_RATE_Gbps                           -- Baudrate in GBit/s
   )
   port map
   (
      i_gtx_ctrl                  => gtx_ctrl_in,
      i_gtx                       => gtx_in,
      o_gtx                       => gtx_out
   );

   -----------------------------------------------------------------------------
   -- RX comma detect
   -----------------------------------------------------------------------------
   rx_comma_detect_inst: entity axis_gtx_bridge_v1_0_lib.rx_comma_detect
   port map
   (
      rxusrclk                    => gtx_out.rx.rxusrclk,
      rxdisperr                   => gtx_out.rx.rxdisperr,
      rxnotintable                => gtx_out.rx.rxnotintable,
      rxcharisk                   => gtx_out.rx.rxcharisk,
      rxdata                      => gtx_out.rx.rxdata,
      --lock info
      comma_lock_axi              => comma_lock_axi,
      comma_lock_plb              => comma_lock_plb,
      comma_lock                  => comma_lock
   );

   o_lock_plb                     <= comma_lock_plb and comma_lock; --ML84 changed
   o_lock_axi                     <= comma_lock_axi and comma_lock; --ML84 added
   o_lock_comma                   <= comma_lock; --ML84 added
   comma_lock_axi_s               <= comma_lock_axi when comma_lock = '1' else --ML84 added
                                     '1'; --Assume AXI while not locked

   -----------------------------------------------------------------------------
   -- RX FIFO
   -----------------------------------------------------------------------------
   rx_gtx_fifo_inst: entity axis_gtx_bridge_v1_0_lib.rx_gtx_fifo
   port map
   (
      --------------------------------------------------------------------------
      -- Debug
      --------------------------------------------------------------------------
      debug_clk                   => open,
      debug                       => rxfifo_debug,
      --------------------------------------------------------------------------
      -- GTX clock side
      --------------------------------------------------------------------------
      -- System
      rxusrclk                    => gtx_out.rx.rxusrclk,
      -- GTX rx
      rxresetdone                 => gtx_out.rx.rxresetdone,
      rxdisperr                   => gtx_out.rx.rxdisperr,
      rxnotintable                => gtx_out.rx.rxnotintable,
      rxcharisk                   => gtx_out.rx.rxcharisk,
      rxdata                      => gtx_out.rx.rxdata,
      -- GTX tx flow control
      rx_xoff                     => rx_xoff,
      tx_xoff                     => tx_xoff,
      --------------------------------------------------------------------------
      -- AXI clock side
      --------------------------------------------------------------------------
      -- System
      aclk                        => gtx_ctrl_in.clk,
      -- FIFO status
      axi_tready                  => rready,
      axi_tvalid                  => rvalid,
      axi_tdata                   => rdata,
      axi_tuser                   => ruser
   );

   m00_axis_tvalid                <= rvalid;
   m00_axis_tdata                 <= rdata;
   m00_axis_tuser                 <= ruser;
   rready                         <= m00_axis_tready;

   -----------------------------------------------------------------------------
   -- TX FIFO
   -----------------------------------------------------------------------------
   s00_axis_tready                <= tready;
   tvalid                         <= s00_axis_tvalid;
   tdata                          <= s00_axis_tdata;
   tuser                          <= s00_axis_tuser;

   tx_gtx_fifo_inst: entity axis_gtx_bridge_v1_0_lib.tx_gtx_fifo
   port map
   (
      --------------------------------------------------------------------------
      -- AXI clock side
      --------------------------------------------------------------------------
      -- System
      aclk                        => gtx_ctrl_in.clk,
      -- AXI read interface
      axi_tready                  => tready,
      axi_tvalid                  => tvalid,
      axi_tdata                   => tdata,
      axi_tuser                   => tuser,
      --------------------------------------------------------------------------
      -- GTX clock side
      --------------------------------------------------------------------------
      -- System
      txusrclk                    => gtx_out.tx.txusrclk,
      -- GTX tx flow control
      comma_lock_axi              => comma_lock_axi_s,
      rx_xoff                     => rx_xoff,
      tx_xoff                     => tx_xoff,
      -- GTX rx
      txresetdone                 => gtx_out.tx.txresetdone,
      txcharisk                   => gtx_in.tx.txcharisk,
      txdata                      => gtx_in.tx.txdata
   );

   -----------------------------------------------------------------------------
   -- GTX connections
   -----------------------------------------------------------------------------
   gtx_in.rx.gtxrxp               <= i_gtx_rx_p;
   gtx_in.rx.gtxrxn               <= i_gtx_rx_n;
   o_gtx_tx_p                     <= gtx_out.tx.gtxtxp;
   o_gtx_tx_n                     <= gtx_out.tx.gtxtxn;

end arch_imp;
