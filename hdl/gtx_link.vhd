--------------------------------------------------------------------------------
--                       Paul Scherrer Institute (PSI)
--------------------------------------------------------------------------------
-- Unit    : gtx_link.vhd
-- Author  : Goran Marinkovic, Section Diagnostic
-- Version : $Revision: 1.3 $
--------------------------------------------------------------------------------
-- Copyright© PSI, Section Diagnostic
--------------------------------------------------------------------------------
-- Comment : This is the package for the GTXE2 component.
--------------------------------------------------------------------------------
-- Std. library (platform) -----------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

package gtx_link_package is

   ---------------------------------------------------------------------------
   -- Type
   ---------------------------------------------------------------------------
   type gtx_pll_out_type is record
      -- QPLL clock Interface
      qpllreset                   : std_logic;
      -- CPLL clock Interface
      cplllock                    : std_logic;
   end record gtx_pll_out_type;

   type gtx_ctrl_in_type is record
      -- System Interface
      clk                         : std_logic;
      rst                         : std_logic;
      -- QPLL clock Interface
      qpllrefclk                  : std_logic;
      qpllclk                     : std_logic;
      qplllock                    : std_logic;
      -- Reference clocks
      gtrefclk0                   : std_logic;
      gtrefclk1                   : std_logic;
   end record gtx_ctrl_in_type;

   type gtx_rx_in_type is record
      -- Power-Down Interface
      rxpd                        : std_logic_vector( 1 downto  0);
      -- Serial lines
      gtxrxp                      : std_logic;
      gtxrxn                      : std_logic;
   end record gtx_rx_in_type;

   type gtx_rx_out_type is record
      -- Clock
      rxusrclk                    : std_logic;
      -- Reset
      rxresetdone                 : std_logic;
      -- Data
      rxdata                      : std_logic_vector(15 downto  0);
      -- Status 8B/10B decoder
      rxbyteisaligned             : std_logic; --ML84
      rxbyterealign               : std_logic; --ML84
      rxcommadet                  : std_logic; --ML84
      rxdisperr                   : std_logic_vector( 1 downto  0);
      rxnotintable                : std_logic_vector( 1 downto  0);
      rxcharisk                   : std_logic_vector( 1 downto  0);
   end record gtx_rx_out_type;

   type gtx_tx_in_type is record
      -- Power-Down Interface
      txpd                        : std_logic_vector( 1 downto  0);
      -- Data
      txdata                      : std_logic_vector(15 downto  0);
      -- Status 8B/10B decoder
      txcharisk                   : std_logic_vector( 1 downto  0);
   end record gtx_tx_in_type;

   type gtx_tx_out_type is record
      -- Clock
      txusrclk                    : std_logic;
      -- Reset
      txresetdone                 : std_logic;
      -- Serial lines
      gtxtxp                      : std_logic;
      gtxtxn                      : std_logic;
   end record gtx_tx_out_type;

   type gtx_in_type is record
      rx                          : gtx_rx_in_type;
      tx                          : gtx_tx_in_type;
   end record gtx_in_type;

   type gtx_quad_in_type is array(0 to 3) of gtx_in_type;

   type gtx_out_type is record
      pll                         : gtx_pll_out_type;
      rx                          : gtx_rx_out_type;
      tx                          : gtx_tx_out_type;
   end record gtx_out_type;

   type gtx_quad_out_type is array(0 to 3) of gtx_out_type;

   -----------------------------------------------------------------------------
   -- GTX sync block
   -----------------------------------------------------------------------------
   component gtx_sync_block is
   generic
   (
      INITIALISE                  : bit_vector(5 downto 0) := "000000"
   );
   port
   (
      clk                         : in    std_logic;                          -- Clock to be synced to
      data_in                     : in    std_logic;                          -- Data to be synced
      data_out                    : out   std_logic                           -- Synced data
   );
   end component gtx_sync_block;

   -----------------------------------------------------------------------------
   -- GTX start up
   -----------------------------------------------------------------------------
   component gtx_startup is
   generic
   (
      SIM_RESET_SPEEDUP           : boolean := FALSE;                         -- Set to "TRUE" to speed up simulation
      EQ_MODE                     : string := "DFE";                          -- RX Equalisation Mode; set to "DFE" or "LPM"
      TX_QPLL_USED                : boolean := FALSE;                         -- Use of QPLL in transmit path
      RX_QPLL_USED                : boolean := FALSE;                         -- Use of QPLL in receive path
      CPU_CLK_MHz                 : real := 125.0;                            -- Frequency of the stable clock in [MHz]
      USR_CLK_MHz                 : real := 125.0;                            -- Frequency of the user clock in [MHz]
      BAUD_RATE_Gbps              : real := 5.0                               -- Baudrate in GBit/s
   );
   port
   (
      --------------------------------------------------------------------------
      -- CPU clock side
      --------------------------------------------------------------------------
      -- System
      CPU_CLK                     : in    std_logic;                          -- Stable Clock
      CPU_RST                     : in    std_logic;                          -- User Reset
      -- QPLL
      QPLLRESET                   : out   std_logic := '0';                   -- Reset QPLL (only if RX uses QPLL)
      -- CPLL
      CPLLRESET                   : out   std_logic := '0';                   -- Reset CPLL (only if RX uses CPLL)
      -- Resets
      GTRXRESET                   : out   std_logic;
      GTTXRESET                   : out   std_logic;
      -- Equalizer
      RXDFEAGCHOLD                : out   std_logic;
      RXDFELFHOLD                 : out   std_logic;
      RXLPMLFHOLD                 : out   std_logic;
      RXLPMHFHOLD                 : out   std_logic;
      -- Status
      RXUSERRDY                   : out   std_logic;
      TXUSERRDY                   : out   std_logic;
      RXRESETDONE_FSM             : out   std_logic;                          -- Reset-sequence has sucessfully been finished.
      TXRESETDONE_FSM             : out   std_logic;                          -- Reset-sequence has sucessfully been finished.
      --------------------------------------------------------------------------
      -- GTX clock side
      --------------------------------------------------------------------------
      -- QPLL
      QPLLLOCK                    : in    std_logic;                          -- Lock Detect from the QPLL of the GT
      -- CPLL
      CPLLLOCK                    : in    std_logic;                          -- Lock Detect from the CPLL of the GT
      -- GTX reset
      RXRESETDONE                 : in    std_logic;
      TXRESETDONE                 : in    std_logic;
      -- RX delay alignment reset
      RXDLYSRESET                 : out   std_logic;
      RXDLYSRESETDONE             : in    std_logic;
      PHALIGNDONE                 : in    std_logic
   );
   end component gtx_startup;

   -----------------------------------------------------------------------------
   -- GTX phase alignment
   -----------------------------------------------------------------------------
   component gtx_qpll is
   port
   (
      GTGREFCLK_IN                : in    std_logic;
      GTNORTHREFCLK0_IN           : in    std_logic;
      GTNORTHREFCLK1_IN           : in    std_logic;
      GTREFCLK1_IN                : in    std_logic;
      GTREFCLK0_IN                : in    std_logic;
      GTSOUTHREFCLK0_IN           : in    std_logic;
      GTSOUTHREFCLK1_IN           : in    std_logic;

      QPLLRESET_IN                : in    std_logic;
      QPLLLOCKDETCLK_IN           : in    std_logic;
      QPLLLOCK_OUT                : out   std_logic;
      QPLLREFCLKLOST_OUT          : out   std_logic;
      QPLLOUTREFCLK_OUT           : out   std_logic;
      QPLLOUTCLK_OUT              : out   std_logic
   );
   end component gtx_qpll;

   -----------------------------------------------------------------------------
   -- GTXE2 wrapper
   -----------------------------------------------------------------------------
   component gtx_link is
   generic
   (
      --------------------------------------------------------------------------
      -- Simulation attributes
      --------------------------------------------------------------------------
      SIM_RESET_SPEEDUP           : boolean := FALSE;                         -- Set to "TRUE" to speed up sim reset
      --------------------------------------------------------------------------
      -- GTX timing
      --------------------------------------------------------------------------
      CPU_CLK_MHz                 : real := 125.0;                            -- Frequency of the stable clock in [MHz]
      REF_CLK_MHz                 : real := 125.0;                            -- Frequency of the reference clock in [MHz]
      BAUD_RATE_Gbps              : real := 5.0                               -- Baudrate in GBit/s
   );
   port
   (
      i_gtx_ctrl                  : in    gtx_ctrl_in_type;
      i_gtx                       : in    gtx_in_type;
      o_gtx                       : out   gtx_out_type
   );
   end component gtx_link;

end package gtx_link_package;

--------------------------------------------------------------------------------
-- GTX sync block
--------------------------------------------------------------------------------
-- Std. library (platform) -----------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Work library (platform) -----------------------------------------------------
library unisim;
use unisim.vcomponents.all;

entity gtx_sync_block is
   generic
   (
      INITIALISE                  : bit_vector(5 downto 0) := "000000"
   );
   port
   (
      clk                         : in    std_logic;                          -- Clock to be synced to
      data_in                     : in    std_logic;                          -- Data to be synced
      data_out                    : out   std_logic                           -- Synced data
   );
end gtx_sync_block;

architecture structural of gtx_sync_block is

   -- Internal Signals
   signal   data_sync             : std_logic_vector( 5 downto  0);

begin

   data_sync( 0)                  <= data_in;

   generate_loop: for i in 0 to 4 generate

      -- These attributes will stop timing errors being reported in back annotated
      -- SDF simulation.
      attribute ASYNC_REG                             : string;
      attribute ASYNC_REG of data_sync_fd_inst        : label is "true";
      -- These attributes will stop XST translating the desired flip-flops into an
      -- SRL based shift register.
      attribute shreg_extract                         : string;
      attribute shreg_extract of data_sync_fd_inst    : label is "no";

   begin

      data_sync_fd_inst: FD
      generic map
      (
         INIT                     => INITIALISE( i)
      )
      port map
      (
         C                        => clk,
         D                        => data_sync( i),
         Q                        => data_sync( i+1)
      );

   end generate;

   data_out                       <= data_sync( 5);

end structural;

--------------------------------------------------------------------------------
-- GTX start up
--------------------------------------------------------------------------------
-- Std. library (platform) -----------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Work library (application) --------------------------------------------------
library axis_gtx_bridge_v1_0_lib;
use axis_gtx_bridge_v1_0_lib.gtx_link_package.all;

entity gtx_startup is
   generic
   (
      SIM_RESET_SPEEDUP           : boolean := FALSE;                         -- Set to "TRUE" to speed up simulation
      EQ_MODE                     : string := "DFE";                          -- RX Equalisation Mode; set to "DFE" or "LPM"
      TX_QPLL_USED                : boolean := FALSE;                         -- Use of QPLL in transmit path
      RX_QPLL_USED                : boolean := FALSE;                         -- Use of QPLL in receive path
      CPU_CLK_MHz                 : real := 125.0;                            -- Frequency of the stable clock in [MHz]
      USR_CLK_MHz                 : real := 125.0;                            -- Frequency of the user clock in [MHz]
      BAUD_RATE_Gbps              : real := 5.0                               -- Baudrate in GBit/s
   );
   port
   (
      --------------------------------------------------------------------------
      -- CPU clock side
      --------------------------------------------------------------------------
      -- System
      CPU_CLK                     : in    std_logic;                          -- Stable Clock
      CPU_RST                     : in    std_logic;                          -- User Reset
      -- QPLL
      QPLLRESET                   : out   std_logic := '0';                   -- Reset QPLL (only if RX uses QPLL)
      -- CPLL
      CPLLRESET                   : out   std_logic := '0';                   -- Reset CPLL (only if RX uses CPLL)
      -- Resets
      GTRXRESET                   : out   std_logic;
      GTTXRESET                   : out   std_logic;
      -- Equalizer
      RXDFEAGCHOLD                : out   std_logic;
      RXDFELFHOLD                 : out   std_logic;
      RXLPMLFHOLD                 : out   std_logic;
      RXLPMHFHOLD                 : out   std_logic;
      -- Status
      RXUSERRDY                   : out   std_logic;
      TXUSERRDY                   : out   std_logic;
      RXRESETDONE_FSM             : out   std_logic;                          -- Reset-sequence has successfully been finished.
      TXRESETDONE_FSM             : out   std_logic;                          -- Reset-sequence has successfully been finished.
      --------------------------------------------------------------------------
      -- GTX clock side
      --------------------------------------------------------------------------
      -- QPLL
      QPLLLOCK                    : in    std_logic;                          -- Lock Detect from the QPLL of the GT
      -- CPLL
      CPLLLOCK                    : in    std_logic;                          -- Lock Detect from the CPLL of the GT
      -- GTX reset
      RXRESETDONE                 : in    std_logic;
      TXRESETDONE                 : in    std_logic;
      -- RX delay alignment reset
      RXDLYSRESET                 : out   std_logic;
      RXDLYSRESETDONE             : in    std_logic;
      PHALIGNDONE                 : in    std_logic
   );
end gtx_startup;

architecture behavioral of gtx_startup is

   type state_type is
   (
      CONFIG,
      ASSERT_RESET,
      PLL_WAIT,
      PLL_LOCK,
      RECCLK_STABLE,
      GT_RESET_DONE,
      PHALIGNMENT_INIT,
      PHALIGNMENT_RESET_DONE,
      PHALIGNMENT_CNT_DONE,
      DONE
   );

   signal   state                 : state_type := CONFIG;

   constant CFG_WAIT_TIME         : real := 510.0;                            -- AR43482: Transceiver needs to wait for at least 500 ns after CONFIG (+10 ns addon)
   constant CFG_WAIT_CYCLES       : integer := integer(CFG_WAIT_TIME / 1000.0 * CPU_CLK_MHz);  -- Number of Clock-Cycles to wait after CONFIG
   signal   cfg_wait_timer        : integer range 0 to CFG_WAIT_CYCLES := CFG_WAIT_CYCLES;

   signal   pllreset              : std_logic_vector( 3 downto  0) := "0000";
   constant PLLRESET_CYCLES       : integer := integer(2000.0 * CPU_CLK_MHz);       -- 2 ms time out
   signal   pllreset_timer        : integer range 0 to PLLRESET_CYCLES := PLLRESET_CYCLES;

   signal   cplllock_sync         : std_logic := '0';
   signal   qplllock_sync         : std_logic := '0';

   constant PLLRESET_LOCK_CYCLES  : integer := 100;                           -- CPLL Lock Time or QPLL Lock Time
   signal   pllreset_lock_timer   : integer range 0 to PLLRESET_LOCK_CYCLES := PLLRESET_LOCK_CYCLES;

   signal   gtreset               : std_logic := '0';
   signal   rxresetdone_sync      : std_logic := '0';
   signal   txresetdone_sync      : std_logic := '0';

   function get_cdrlock_time(SIM_RESET_SPEEDUP: in boolean) return real is
      variable lock_time          : real;
   begin
      if (SIM_RESET_SPEEDUP = TRUE) then
         lock_time                := 100.0;
      else
         lock_time                := 50000.0 / BAUD_RATE_Gbps;                      --Typical CDR lock time is 50,000UI as per DS183
      end if;
      return lock_time;
   end function;

   constant RX_CDRLOCK_TIME       : real := get_cdrlock_time(SIM_RESET_SPEEDUP);
   constant RX_CDRLOCK_CYCLES     : integer := integer(RX_CDRLOCK_TIME / 1000.0 * CPU_CLK_MHz);
   signal   rx_cdrlock_cnt        : integer range 0 to RX_CDRLOCK_CYCLES := RX_CDRLOCK_CYCLES;

   signal   userrdy               : std_logic := '0';

   constant RXDLYSRESET_CYCLES    : integer := integer(30.0 / 1000.0 * CPU_CLK_MHz); -- The datasheet states duration RXDLYSRESET < 50 ns
   signal   rxdlysreset_cnt       : integer range 0 to RXDLYSRESET_CYCLES := 0;

   constant PHALIGNMENT_CYCLES    : integer := integer(5000.0 * CPU_CLK_MHz / USR_CLK_MHz); --5000 RXUSRCLK cycles is the max time for Multi lanes designs
   signal   phalignment_timer     : integer range 0 to PHALIGNMENT_CYCLES := PHALIGNMENT_CYCLES;

   signal   rxdlysresetdone_sync  : std_logic := '0';

   signal   phaligndone_sync      : std_logic := '0';
   signal   phaligndone_sync_d    : std_logic := '0';
   signal   phaligndone_cnt       : std_logic_vector( 1 downto  0) := "00";

   constant DFE_WAIT              : integer := integer((37000.0 / BAUD_RATE_Gbps) * CPU_CLK_MHz);
   signal   dfe_wait_timer        : integer range 0 to DFE_WAIT := DFE_WAIT;

begin

   -----------------------------------------------------------------------------
   -- FSM
   -----------------------------------------------------------------------------
   state_proc: process(CPU_CLK)
   begin
      if rising_edge(CPU_CLK) then
         if (CPU_RST = '1' ) then
            state                 <= CONFIG;
         elsif ((pllreset_timer    = 0) or
                (phalignment_timer = 0)) then
            state                 <= ASSERT_RESET;
         else
            case state is
            when CONFIG =>
               if (cfg_wait_timer = 0) then
                 state            <= ASSERT_RESET;
               end if;

            when ASSERT_RESET =>
               state              <= PLL_WAIT;

            when  PLL_WAIT =>
               if (pllreset_lock_timer = 0) then
                  state           <=  PLL_LOCK;
               end if;

            when PLL_LOCK =>
               if (((    RX_QPLL_USED and (qplllock_sync = '1')) or
                    (not RX_QPLL_USED and (cplllock_sync = '1'))) and
                   ((    TX_QPLL_USED and (qplllock_sync = '1')) or
                    (not TX_QPLL_USED and (cplllock_sync = '1')))) then
                 state            <= RECCLK_STABLE;
               end if;

            when RECCLK_STABLE =>
               if (rx_cdrlock_cnt = 0) then
                  state           <= GT_RESET_DONE;
               end if;

            when GT_RESET_DONE =>
               if ((rxresetdone_sync = '1') and
                   (txresetdone_sync = '1')) then
                  state           <= PHALIGNMENT_INIT;
               end if;

            when PHALIGNMENT_INIT =>
               state              <= PHALIGNMENT_RESET_DONE;

            when PHALIGNMENT_RESET_DONE =>
               if (rxdlysresetdone_sync = '1') then
                  state           <= PHALIGNMENT_CNT_DONE;
               end if;

            when PHALIGNMENT_CNT_DONE =>
               if (phaligndone_cnt( 1) = '1') then
                  state           <= DONE;
               end if;

            when DONE =>
               null;

            when others =>
               state              <= CONFIG;
            end case;
         end if;
      end if;
   end process state_proc;

   -----------------------------------------------------------------------------
   -- AR43482: Transceiver needs to wait for 500 ns after CONFIG
   -----------------------------------------------------------------------------
   process(CPU_CLK)
   begin
      if rising_edge(CPU_CLK) then
         if (state = CONFIG) then
            cfg_wait_timer        <= cfg_wait_timer - 1;
         else
            cfg_wait_timer        <= CFG_WAIT_CYCLES;
         end if;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- Time out
   -----------------------------------------------------------------------------
   process(CPU_CLK)
   begin
      if rising_edge(CPU_CLK) then
         if ((state = PLL_LOCK) or
             (state = GT_RESET_DONE  )) then
            pllreset_timer        <= pllreset_timer - 1;
         else
            pllreset_timer        <= PLLRESET_CYCLES;
         end if;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- Assert PLL reset for min. 1 reference clock period
   -----------------------------------------------------------------------------
   process(CPU_CLK)
   begin
      if rising_edge(CPU_CLK) then
         if (state = ASSERT_RESET) then
            pllreset              <= "1111";
         else
            pllreset              <= pllreset( 2 downto  0) & '0';
         end if;
      end if;
   end process;

   CPLLRESET                      <= pllreset( 3) when ((RX_QPLL_USED = FALSE) or (TX_QPLL_USED = FALSE)) else '0';
   QPLLRESET                      <= pllreset( 3) when ((RX_QPLL_USED = TRUE ) or (TX_QPLL_USED = TRUE )) else '0';

   -----------------------------------------------------------------------------
   -- Wait for CPLL lock time or QPLL lock time after reset
   -----------------------------------------------------------------------------
   process(CPU_CLK)
   begin
      if rising_edge(CPU_CLK) then
         if (pllreset( 3) = '1') then
            pllreset_lock_timer   <= PLLRESET_LOCK_CYCLES;
         elsif (pllreset_lock_timer > 0) then
            pllreset_lock_timer   <= pllreset_lock_timer - 1;
         end if;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- Clock Domain Crossing lock signals
   -----------------------------------------------------------------------------
   qplllock_sync_gtx_sync_block_inst: entity axis_gtx_bridge_v1_0_lib.gtx_sync_block
   port map
   (
      clk                         => CPU_CLK,
      data_in                     => QPLLLOCK,
      data_out                    => qplllock_sync
   );

   cplllock_sync_gtx_sync_block_inst : entity axis_gtx_bridge_v1_0_lib.gtx_sync_block
   port map
   (
      clk                         => CPU_CLK,
      data_in                     => CPLLLOCK,
      data_out                    => cplllock_sync
   );

   -----------------------------------------------------------------------------
   -- GTX reset
   -----------------------------------------------------------------------------
   process(CPU_CLK)
   begin
      if rising_edge(CPU_CLK) then
         if ((state = ASSERT_RESET) or
             (state = PLL_WAIT) or
             (state = PLL_LOCK)) then
            gtreset               <= '1';
         else
            gtreset               <= '0';
         end if;
      end if;
   end process;

   GTRXRESET                      <= gtreset;
   GTTXRESET                      <= gtreset;

   -----------------------------------------------------------------------------
   -- User ready
   -----------------------------------------------------------------------------
   process(CPU_CLK)
   begin
      if rising_edge(CPU_CLK) then
         if ((state = CONFIG              ) or
             (state = ASSERT_RESET   ) or
             (state = PLL_WAIT   ) or
             (state = PLL_LOCK   ) or
             (state = RECCLK_STABLE)) then
            userrdy               <= '0';
         else
            userrdy               <= '1';
         end if;
      end if;
   end process;

   RXUSERRDY                      <= userrdy;
   TXUSERRDY                      <= userrdy;

   -----------------------------------------------------------------------------
   -- CDR locked status
   -----------------------------------------------------------------------------
   process(CPU_CLK)
   begin
      if rising_edge(CPU_CLK) then
         if (gtreset = '1') then
            rx_cdrlock_cnt        <= RX_CDRLOCK_CYCLES;
         elsif (rx_cdrlock_cnt /= 0) then
            rx_cdrlock_cnt        <= rx_cdrlock_cnt - 1;
         end if;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- Clock Domain Crossing reset done signal
   -----------------------------------------------------------------------------
   rxresetdone_gtx_sync_block_inst: entity axis_gtx_bridge_v1_0_lib.gtx_sync_block
   port map
   (
      clk                         => CPU_CLK,
      data_in                     => RXRESETDONE,
      data_out                    => rxresetdone_sync
   );

   txresetdone_gtx_sync_block_inst: entity axis_gtx_bridge_v1_0_lib.gtx_sync_block
   port map
   (
      clk                         => CPU_CLK,
      data_in                     => TXRESETDONE,
      data_out                    => txresetdone_sync
   );

   -----------------------------------------------------------------------------
   -- Phase alignment timeout
   -----------------------------------------------------------------------------
   process(CPU_CLK)
   begin
      if rising_edge(CPU_CLK) then
         if ((state = PHALIGNMENT_RESET_DONE) or
             (state = PHALIGNMENT_CNT_DONE)) then
            phalignment_timer     <= phalignment_timer - 1;
         else
            phalignment_timer     <= PHALIGNMENT_CYCLES;
         end if;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- Phase alignment DLYRESET pulse
   -----------------------------------------------------------------------------
   process(CPU_CLK)
   begin
      if rising_edge(CPU_CLK) then
         if (state = PHALIGNMENT_INIT) then
            rxdlysreset_cnt       <= RXDLYSRESET_CYCLES;
         else
            if (rxdlysreset_cnt /= 0) then
               rxdlysreset_cnt    <= rxdlysreset_cnt - 1;
            end if;
         end if;
      end if;
   end process;

   RXDLYSRESET                    <= '0' when (rxdlysreset_cnt = 0) else '1';

   -----------------------------------------------------------------------------
   -- Phase alignment DLYRESET done
   -----------------------------------------------------------------------------
   gtx_sync_block_dlysresetdone: entity axis_gtx_bridge_v1_0_lib.gtx_sync_block
   port map
   (
      clk                         => CPU_CLK,
      data_in                     => RXDLYSRESETDONE,
      data_out                    => rxdlysresetdone_sync
   );

   -----------------------------------------------------------------------------
   -- Phase alignment data
   -----------------------------------------------------------------------------
   gtx_sync_block_phaligndone: entity axis_gtx_bridge_v1_0_lib.gtx_sync_block
   port map
   (
      clk                         =>  CPU_CLK,
      data_in                     =>  PHALIGNDONE,
      data_out                    =>  phaligndone_sync
   );

   process(CPU_CLK)
   begin
      if rising_edge(CPU_CLK) then
         phaligndone_sync_d       <= phaligndone_sync;
      end if;
   end process;

   process(CPU_CLK)
   begin
      if rising_edge(CPU_CLK) then
         if (state = PHALIGNMENT_CNT_DONE) then
            if (phaligndone_sync_d = '0') and (phaligndone_sync = '1') then
               phaligndone_cnt    <= phaligndone_cnt( 0) & '1';
            end if;
         else
            phaligndone_cnt       <= "00";
         end if;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- DFE adjustment time
   -----------------------------------------------------------------------------
   dfe_wait_sim: if (SIM_RESET_SPEEDUP = TRUE) generate

      dfe_wait_timer                <= 0;

   end generate;

   dfe_wait_hw: if (SIM_RESET_SPEEDUP = FALSE) generate

      process(CPU_CLK)
      begin
         if rising_edge(CPU_CLK) then
            if ((state = CONFIG           ) or
                (state = ASSERT_RESET) or
                (state = PLL_WAIT) or
                (state = PLL_LOCK)) then
               dfe_wait_timer       <= DFE_WAIT;
            else
               if (dfe_wait_timer /= 0) then
                  dfe_wait_timer    <= dfe_wait_timer - 1;
               end if;
            end if;
         end if;
      end process;

   end generate;

   -----------------------------------------------------------------------------
   -- DFE adjustment hold
   -----------------------------------------------------------------------------
   process(CPU_CLK)
   begin
      if rising_edge(CPU_CLK) then
         if ((EQ_MODE = "DFE") and (state = DONE) and (dfe_wait_timer = 0)) then
            RXDFEAGCHOLD          <= '1';
            RXDFELFHOLD           <= '1';
            RXLPMHFHOLD           <= '0';
            RXLPMLFHOLD           <= '0';
         else
            RXDFEAGCHOLD          <= '0';
            RXDFELFHOLD           <= '0';
            RXLPMHFHOLD           <= '0';
            RXLPMLFHOLD           <= '0';
         end if;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- FSM status
   -----------------------------------------------------------------------------
   RXRESETDONE_FSM                <= '1' when (state = DONE) else '0';
   TXRESETDONE_FSM                <= '1' when (state = DONE) else '0';

end behavioral;

--------------------------------------------------------------------------------
-- GTX qpll
--------------------------------------------------------------------------------
-- Std. library (platform) -----------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Work library (platform) -----------------------------------------------------
library unisim;
use unisim.vcomponents.all;

entity gtx_qpll is
   port
   (
      GTGREFCLK_IN                : in    std_logic;
      GTNORTHREFCLK0_IN           : in    std_logic;
      GTNORTHREFCLK1_IN           : in    std_logic;
      GTREFCLK1_IN                : in    std_logic;
      GTREFCLK0_IN                : in    std_logic;
      GTSOUTHREFCLK0_IN           : in    std_logic;
      GTSOUTHREFCLK1_IN           : in    std_logic;

      QPLLRESET_IN                : in    std_logic;
      QPLLLOCKDETCLK_IN           : in    std_logic;
      QPLLLOCK_OUT                : out   std_logic;
      QPLLREFCLKLOST_OUT          : out   std_logic;
      QPLLOUTREFCLK_OUT           : out   std_logic;
      QPLLOUTCLK_OUT              : out   std_logic
   );
end gtx_qpll;

architecture structural of gtx_qpll is

   signal   qpllpd_wait           : std_logic_vector( 95 downto  0) := X"FFFFFFFFFFFFFFFFFFFFFFFF";
   signal   qpllreset_wait        : std_logic_vector(127 downto  0) := X"000000000000000000000000000000FF";

   attribute equivalent_register_removal: string;
   attribute equivalent_register_removal of qpllpd_wait    : signal is "no";
   attribute equivalent_register_removal of qpllreset_wait : signal is "no";

   signal   qpllpd                : std_logic;
   signal   qpllreset_r           : std_logic;
   signal   qpllreset_r2          : std_logic;
   signal   qpllreset             : std_logic;

   attribute ASYNC_REG                   : string;
   attribute ASYNC_REG of qpllreset_r    : signal is "TRUE";
   attribute ASYNC_REG of qpllreset_r2   : signal is "TRUE";

begin

   process(GTREFCLK0_IN)
   begin
      if rising_edge(GTREFCLK0_IN) then
         qpllpd_wait              <= qpllpd_wait(94 downto 0) & '0';
         qpllreset_wait           <= qpllreset_wait(126 downto 0) & '0';
       end if;
   end process;

   qpllpd                         <= qpllpd_wait(95);

   process(GTREFCLK0_IN)
   begin
      if rising_edge(GTREFCLK0_IN) then
         qpllreset_r              <= QPLLRESET_IN;
         qpllreset_r2             <= qpllreset_r ;
      end if;
   end process;

   qpllreset                      <= qpllreset_r2 or qpllreset_wait(127);

   gtxe2_common_inst: GTXE2_COMMON
   generic map
   (
      ------------------- Simulation attributes --------------------------------
      SIM_RESET_SPEEDUP           => "TRUE",
      SIM_QPLLREFCLK_SEL          => "001",
      SIM_VERSION                 => "4.0",
      ------------------ Common block attributes -------------------------------
      BIAS_CFG                    => X"0000040000001000",
      COMMON_CFG                  => X"00000000",
      QPLL_CFG                    => X"06801C1",
      QPLL_CLKOUT_CFG             => "0000",
      QPLL_COARSE_FREQ_OVRD       => "010000",
      QPLL_COARSE_FREQ_OVRD_EN    => '0',
      QPLL_CP                     => "0000011111",
      QPLL_CP_MONITOR_EN          => '0',
      QPLL_DMONITOR_SEL           => '0',
      QPLL_FBDIV                  => "0000100000",
      QPLL_FBDIV_MONITOR_EN       => '0',
      QPLL_FBDIV_RATIO            => '1',
      QPLL_INIT_CFG               => X"000006",
      QPLL_LOCK_CFG               => X"21E8",
      QPLL_LPF                    => "1111",
      QPLL_REFCLK_DIV             => 1
   )
   port map
   (
      ------------- Common Block  - Dynamic Reconfiguration Port (DRP) ---------
      DRPADDR                     => X"00",
      DRPCLK                      => '0',
      DRPDI                       => X"0000",
      DRPDO                       => open,
      DRPEN                       => '0',
      DRPRDY                      => open,
      DRPWE                       => '0',
      ---------------------- Common Block  - Ref Clock Ports -------------------
      GTGREFCLK                   => GTGREFCLK_IN,
      GTNORTHREFCLK0              => GTNORTHREFCLK0_IN,
      GTNORTHREFCLK1              => GTNORTHREFCLK1_IN,
      GTREFCLK0                   => GTREFCLK0_IN,
      GTREFCLK1                   => GTREFCLK1_IN,
      GTSOUTHREFCLK0              => GTSOUTHREFCLK0_IN,
      GTSOUTHREFCLK1              => GTSOUTHREFCLK1_IN,
      ------------------------- Common Block -  QPLL Ports ---------------------
      QPLLDMONITOR                => open,
      ----------------------- Common Block - Clocking Ports --------------------
      QPLLOUTCLK                  => QPLLOUTCLK_OUT,
      QPLLOUTREFCLK               => QPLLOUTREFCLK_OUT,
      REFCLKOUTMONITOR            => open,
      ------------------------- Common Block - QPLL Ports ----------------------
      QPLLFBCLKLOST               => open,
      QPLLLOCK                    => QPLLLOCK_OUT,
      QPLLLOCKDETCLK              => QPLLLOCKDETCLK_IN,
      QPLLLOCKEN                  => '1',
      QPLLOUTRESET                => '0',
      QPLLPD                      => qpllpd,
      QPLLREFCLKLOST              => QPLLREFCLKLOST_OUT,
      QPLLREFCLKSEL               => "001",
      QPLLRESET                   => qpllreset,
      QPLLRSVD1                   => "0000000000000000",
      QPLLRSVD2                   => "11111",
      --------------------------------- QPLL Ports -----------------------------
      BGBYPASSB                   => '1',
      BGMONITORENB                => '1',
      BGPDB                       => '1',
      BGRCALOVRD                  => "11111",
      PMARSVD                     => "00000000",
      RCALENB                     => '1'
   );

end structural;

--------------------------------------------------------------------------------
-- GTXE2 wrapper.
--------------------------------------------------------------------------------
-- Std. library (platform) -----------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Work library (platform) -----------------------------------------------------
library unisim;
use unisim.vcomponents.all;

-- Work library (application) --------------------------------------------------
library axis_gtx_bridge_v1_0_lib;
use axis_gtx_bridge_v1_0_lib.gtx_link_package.all;

entity gtx_link is
   generic
   (
      --------------------------------------------------------------------------
      -- Simulation attributes
      --------------------------------------------------------------------------
      SIM_RESET_SPEEDUP           : boolean := FALSE;                         -- Set to "TRUE" to speed up sim reset
      --------------------------------------------------------------------------
      -- GTX timing
      --------------------------------------------------------------------------
      CPU_CLK_MHz                 : real := 125.0;                            -- Frequency of the stable clock in [MHz]
      REF_CLK_MHz                 : real := 125.0;                            -- Frequency of the reference clock in [MHz]
      BAUD_RATE_Gbps              : real := 5.0                               -- Baudrate in GBit/s
   );
   port
   (
      i_gtx_ctrl                  : in    gtx_ctrl_in_type;
      i_gtx                       : in    gtx_in_type;
      o_gtx                       : out   gtx_out_type
   );
end gtx_link;

architecture structural of gtx_link is

   -- CPLL signals
   signal   cpllpd_wait           : std_logic_vector( 95 downto  0) := X"FFFFFFFFFFFFFFFFFFFFFFFF";
   attribute equivalent_register_removal: string;
   attribute equivalent_register_removal of cpllpd_wait : signal is "no";

   signal   cpllpd                : std_logic ;
   signal   cpllreset             : std_logic ;
   signal   cplllock              : std_logic;
   -- Receiver signals
   signal   gtrxreset             : std_logic;
   signal   rxresetdone           : std_logic;
   signal   rxoutclk              : std_logic;
   signal   rxusrclk              : std_logic;
   signal   rxdlysreset           : std_logic;
   signal   rxdlysresetdone       : std_logic;
   signal   rxphaligndone         : std_logic;
   signal   rxdfeagchold          : std_logic;
   signal   rxdfelfhold           : std_logic;
   signal   rxlpmlfhold           : std_logic;
   signal   rxlpmhfhold           : std_logic;
   signal   rxuserrdy             : std_logic;
   signal   rxdata                : std_logic_vector(63 downto  0);
   signal   rxcharisk             : std_logic_vector( 7 downto  0);
   signal   rxdisperr             : std_logic_vector( 7 downto  0);
   signal   rxnotintable          : std_logic_vector( 7 downto  0);
   -- Transmitter signals
   signal   gttxreset             : std_logic;
   signal   txresetdone           : std_logic;
   signal   txoutclk              : std_logic;
   signal   txusrclk              : std_logic;
   signal   txuserrdy             : std_logic;
   signal   txdata                : std_logic_vector(63 downto  0);
   signal   txcharisk             : std_logic_vector( 7 downto  0);

   function PARAM_CPLL_FBDIV_45(REF_CLK_MHz : real; BAUD_RATE_Gbps : real) return integer is
      variable CPLL_FBDIV_45      : integer;
   begin
      if (REF_CLK_MHz = 125.0) then
         if    (BAUD_RATE_Gbps = 1.25) then
            CPLL_FBDIV_45         := 5;
         elsif (BAUD_RATE_Gbps = 2.5) then
            CPLL_FBDIV_45         := 5;
         elsif (BAUD_RATE_Gbps = 3.125) then
            CPLL_FBDIV_45         := 5;
         elsif (BAUD_RATE_Gbps = 5.0) then
            CPLL_FBDIV_45         := 5;
         else
            CPLL_FBDIV_45         := 5;
            assert false report "Invalid GTX baud rate = " & real'image(BAUD_RATE_Gbps) severity error;
         end if;
      else
         CPLL_FBDIV_45            := 5;
         assert false report "Invalid GTX reference frequency = " & real'image(REF_CLK_MHz) severity error;
      end if;
      return CPLL_FBDIV_45;
   end PARAM_CPLL_FBDIV_45;

   function PARAM_CPLL_FBDIV(REF_CLK_MHz : real; BAUD_RATE_Gbps : real) return integer is
      variable CPLL_FBDIV         : integer;
   begin
      if (REF_CLK_MHz = 125.0) then
         if    (BAUD_RATE_Gbps = 1.25) then
            CPLL_FBDIV            := 4;
         elsif (BAUD_RATE_Gbps = 2.5) then
            CPLL_FBDIV            := 4;
         elsif (BAUD_RATE_Gbps = 3.125) then
            CPLL_FBDIV            := 5;
         elsif (BAUD_RATE_Gbps = 5.0) then
            CPLL_FBDIV            := 4;
         else
            CPLL_FBDIV            := 4;
            assert false report "Invalid GTX baud rate = " & real'image(BAUD_RATE_Gbps) severity error;
         end if;
      else
         CPLL_FBDIV               := 4;
         assert false report "Invalid GTX reference frequency = " & real'image(REF_CLK_MHz) severity error;
      end if;
      return CPLL_FBDIV;
   end PARAM_CPLL_FBDIV;

   function PARAM_CPLL_REFCLK_DIV(REF_CLK_MHz : real; BAUD_RATE_Gbps : real) return integer is
      variable CPLL_REFCLK_DIV    : integer;
   begin
      if (REF_CLK_MHz = 125.0) then
         if    (BAUD_RATE_Gbps = 1.25) then
            CPLL_REFCLK_DIV       := 1;
         elsif (BAUD_RATE_Gbps = 2.5) then
            CPLL_REFCLK_DIV       := 1;
         elsif (BAUD_RATE_Gbps = 3.125) then
            CPLL_REFCLK_DIV       := 1;
         elsif (BAUD_RATE_Gbps = 5.0) then
            CPLL_REFCLK_DIV       := 1;
         else
            CPLL_REFCLK_DIV       := 1;
            assert false report "Invalid GTX baud rate = " & real'image(BAUD_RATE_Gbps) severity error;
         end if;
      else
         CPLL_REFCLK_DIV          := 1;
         assert false report "Invalid GTX reference frequency = " & real'image(REF_CLK_MHz) severity error;
      end if;
      return CPLL_REFCLK_DIV;
   end PARAM_CPLL_REFCLK_DIV;

   function PARAM_RXTXOUT_DIV(REF_CLK_MHz : real; BAUD_RATE_Gbps : real) return integer is
      variable RXTXOUT_DIV        : integer;
   begin
      if (REF_CLK_MHz = 125.0) then
         if    (BAUD_RATE_Gbps = 1.25) then
            RXTXOUT_DIV           := 4;
         elsif (BAUD_RATE_Gbps = 2.5) then
            RXTXOUT_DIV           := 2;
         elsif (BAUD_RATE_Gbps = 3.125) then
            RXTXOUT_DIV           := 2;
         elsif (BAUD_RATE_Gbps = 5.0) then
            RXTXOUT_DIV           := 1;
         else
            RXTXOUT_DIV           := 4;
            assert false report "Invalid GTX baud rate = " & real'image(BAUD_RATE_Gbps) severity error;
         end if;
      else
         RXTXOUT_DIV              := 4;
         assert false report "Invalid GTX reference frequency = " & real'image(REF_CLK_MHz) severity error;
      end if;
      return RXTXOUT_DIV;
   end PARAM_RXTXOUT_DIV;

   function PARAM_USRCLK(BAUD_RATE_Gbps : real) return real is
   begin
      return (BAUD_RATE_Gbps * 1000.0 / 20.0);
   end PARAM_USRCLK;

   constant CPLL_FBDIV_45         : integer := PARAM_CPLL_FBDIV_45(REF_CLK_MHz, BAUD_RATE_Gbps);
   constant CPLL_FBDIV            : integer := PARAM_CPLL_FBDIV(REF_CLK_MHz, BAUD_RATE_Gbps);
   constant CPLL_REFCLK_DIV       : integer := PARAM_CPLL_REFCLK_DIV(REF_CLK_MHz, BAUD_RATE_Gbps);
   constant RXOUT_DIV             : integer := PARAM_RXTXOUT_DIV(REF_CLK_MHz, BAUD_RATE_Gbps);
   constant TXOUT_DIV             : integer := PARAM_RXTXOUT_DIV(REF_CLK_MHz, BAUD_RATE_Gbps);
   constant USR_CLK_MHz           : real := PARAM_USRCLK(BAUD_RATE_Gbps);

   --ML84
   signal rxbyteisaligned, rxbyterealign, rxcommadet : std_logic;

begin

   -----------------------------------------------------------------------------
   -- CPLL status
   -----------------------------------------------------------------------------
   o_gtx.pll.cplllock            <= cplllock;

   process(i_gtx_ctrl.gtrefclk0)
   begin
      if rising_edge(i_gtx_ctrl.gtrefclk0) then
         cpllpd_wait              <= cpllpd_wait(94 downto 0) & '0';
      end if;
   end process;

   cpllpd                         <= cpllpd_wait(95);

   -----------------------------------------------------------------------------
   -- User clocks
   -----------------------------------------------------------------------------
   rxoutclk_bufg1_i : BUFG
   port map
   (
      I                           => rxoutclk,
      O                           => rxusrclk
   );

   o_gtx.rx.rxusrclk              <= rxusrclk;

   txusrclk_bufg_inst: BUFG
   port map
   (
      I                           => txoutclk,
      O                           => txusrclk
   );

   o_gtx.tx.txusrclk              <= txusrclk;

   -----------------------------------------------------------------------------
   -- Start-up FSM
   -----------------------------------------------------------------------------
   gtx_startup_inst: entity axis_gtx_bridge_v1_0_lib.gtx_startup
   generic map
   (
      SIM_RESET_SPEEDUP           => SIM_RESET_SPEEDUP,
      EQ_MODE                     => "DFE",
      TX_QPLL_USED                => FALSE,
      RX_QPLL_USED                => FALSE,
      CPU_CLK_MHz                 => CPU_CLK_MHz,
      USR_CLK_MHz                 => USR_CLK_MHz,
      BAUD_RATE_Gbps              => BAUD_RATE_Gbps
   )
   port map
   (
      --------------------------------------------------------------------------
      -- CPU clock side
      --------------------------------------------------------------------------
      -- System
      CPU_CLK                     => i_gtx_ctrl.clk,
      CPU_RST                     => i_gtx_ctrl.rst,
      -- QPLL
      QPLLRESET                   => o_gtx.pll.qpllreset,
      -- CPLL
      CPLLRESET                   => cpllreset,
      -- Resets
      GTRXRESET                   => gtrxreset,
      GTTXRESET                   => gttxreset,
      -- Equalizer
      RXDFEAGCHOLD                => rxdfeagchold,
      RXDFELFHOLD                 => rxdfelfhold,
      RXLPMLFHOLD                 => rxlpmlfhold,
      RXLPMHFHOLD                 => rxlpmhfhold,
      -- Status
      RXUSERRDY                   => rxuserrdy,
      TXUSERRDY                   => txuserrdy,
      RXRESETDONE_FSM             => o_gtx.rx.rxresetdone,
      TXRESETDONE_FSM             => o_gtx.tx.txresetdone,
      --------------------------------------------------------------------------
      -- GTX clock side
      --------------------------------------------------------------------------
      -- QPLL
      QPLLLOCK                    => i_gtx_ctrl.qplllock,
      -- CPLL
      CPLLLOCK                    => cplllock,
      -- GTX reset
      RXRESETDONE                 => rxresetdone,
      TXRESETDONE                 => txresetdone,
      -- RX delay alignment reset
      RXDLYSRESET                 => rxdlysreset,
      RXDLYSRESETDONE             => rxdlysresetdone,
      PHALIGNDONE                 => rxphaligndone
   );

   -----------------------------------------------------------------------------
   -- Receiver mapping
   -----------------------------------------------------------------------------
   o_gtx.rx.rxcharisk             <= rxcharisk(1 downto 0);
   o_gtx.rx.rxdata                <= rxdata(15 downto 0);
   o_gtx.rx.rxdisperr             <= rxdisperr(1 downto 0);
   o_gtx.rx.rxnotintable          <= rxnotintable(1 downto 0);
   o_gtx.rx.rxbyteisaligned       <= rxbyteisaligned;
   o_gtx.rx.rxbyterealign         <= rxbyterealign;  
   o_gtx.rx.rxcommadet            <= rxcommadet; 

   -----------------------------------------------------------------------------
   -- Transmitter mapping
   -----------------------------------------------------------------------------
   txdata                         <= X"000000000000" & i_gtx.tx.txdata;
   txcharisk                      <= "000000" & i_gtx.tx.txcharisk;

   -----------------------------------------------------------------------------
   -- GTXE2 instance
   -----------------------------------------------------------------------------
   gtxe2_channel_inst: GTXE2_CHANNEL
   generic map
   (
      --_______________________ Simulation-Only Attributes ___________________
      SIM_RECEIVER_DETECT_PASS    => "TRUE",
      SIM_RESET_SPEEDUP           => "TRUE",
      SIM_TX_EIDLE_DRIVE_LEVEL    => "X",
      SIM_CPLLREFCLK_SEL          => "001",
      SIM_VERSION                 => "4.0",
      ------------------RX Byte and Word Alignment Attributes---------------
      ALIGN_COMMA_DOUBLE          => "FALSE",
      ALIGN_COMMA_ENABLE          => "0011111111",
      ALIGN_COMMA_WORD            => 2,
      ALIGN_MCOMMA_DET            => "TRUE",
      ALIGN_MCOMMA_VALUE          => "1010000011",
      ALIGN_PCOMMA_DET            => "TRUE",
      ALIGN_PCOMMA_VALUE          => "0101111100",
      SHOW_REALIGN_COMMA          => "TRUE",
      RXSLIDE_AUTO_WAIT           => 7,
      RXSLIDE_MODE                => "PCS",
      RX_SIG_VALID_DLY            => 10,
      ------------------RX 8B/10B Decoder Attributes---------------
      RX_DISPERR_SEQ_MATCH        => "TRUE",
      DEC_MCOMMA_DETECT           => "TRUE",
      DEC_PCOMMA_DETECT           => "TRUE",
      DEC_VALID_COMMA_ONLY        => "FALSE",
      ------------------------RX Clock Correction Attributes----------------------
      CBCC_DATA_SOURCE_SEL        => "DECODED",
      CLK_COR_SEQ_2_USE           => "FALSE",
      CLK_COR_KEEP_IDLE           => "FALSE",
      CLK_COR_MAX_LAT             => 10,
      CLK_COR_MIN_LAT             => 8,
      CLK_COR_PRECEDENCE          => "TRUE",
      CLK_COR_REPEAT_WAIT         => 0,
      CLK_COR_SEQ_LEN             => 1,
      CLK_COR_SEQ_1_ENABLE        => "1111",
      CLK_COR_SEQ_1_1             => "0100000000",
      CLK_COR_SEQ_1_2             => "0000000000",
      CLK_COR_SEQ_1_3             => "0000000000",
      CLK_COR_SEQ_1_4             => "0000000000",
      CLK_CORRECT_USE             => "FALSE",
      CLK_COR_SEQ_2_ENABLE        => "1111",
      CLK_COR_SEQ_2_1             => "0100000000",
      CLK_COR_SEQ_2_2             => "0000000000",
      CLK_COR_SEQ_2_3             => "0000000000",
      CLK_COR_SEQ_2_4             => "0000000000",
      ------------------------RX Channel Bonding Attributes----------------------
      CHAN_BOND_KEEP_ALIGN        => "FALSE",
      CHAN_BOND_MAX_SKEW          => 1,
      CHAN_BOND_SEQ_LEN           => 1,
      CHAN_BOND_SEQ_1_1           => "0000000000",
      CHAN_BOND_SEQ_1_2           => "0000000000",
      CHAN_BOND_SEQ_1_3           => "0000000000",
      CHAN_BOND_SEQ_1_4           => "0000000000",
      CHAN_BOND_SEQ_1_ENABLE      => "1111",
      CHAN_BOND_SEQ_2_1           => "0000000000",
      CHAN_BOND_SEQ_2_2           => "0000000000",
      CHAN_BOND_SEQ_2_3           => "0000000000",
      CHAN_BOND_SEQ_2_4           => "0000000000",
      CHAN_BOND_SEQ_2_ENABLE      => "1111",
      CHAN_BOND_SEQ_2_USE         => "FALSE",
      FTS_DESKEW_SEQ_ENABLE       => "1111",
      FTS_LANE_DESKEW_CFG         => "1111",
      FTS_LANE_DESKEW_EN          => "FALSE",
      ---------------------------RX Margin Analysis Attributes----------------------------
      ES_CONTROL                  => "000000",
      ES_ERRDET_EN                => "FALSE",
      ES_EYE_SCAN_EN              => "TRUE",
      ES_HORZ_OFFSET              => X"000",
      ES_PMA_CFG                  => "0000000000",
      ES_PRESCALE                 => "00000",
      ES_QUALIFIER                => X"00000000000000000000",
      ES_QUAL_MASK                => X"00000000000000000000",
      ES_SDATA_MASK               => X"00000000000000000000",
      ES_VERT_OFFSET              => "000000000",
      -------------------------FPGA RX Interface Attributes-------------------------
      RX_DATA_WIDTH               => 20,
      ---------------------------PMA Attributes----------------------------
      OUTREFCLK_SEL_INV           => "11",
      PMA_RSV                     => X"00018480",
      PMA_RSV2                    => X"2050",
      PMA_RSV3                    => "00",
      PMA_RSV4                    => X"00000000",
      RX_BIAS_CFG                 => "000000000100",
      DMONITOR_CFG                => X"000A00",
      RX_CM_SEL                   => "11",
      RX_CM_TRIM                  => "010",
      RX_DEBUG_CFG                => "000000000000",
      RX_OS_CFG                   => "0000010000000",
      TERM_RCAL_CFG               => "10000",
      TERM_RCAL_OVRD              => '0',
      TST_RSV                     => X"00000000",
      RX_CLK25_DIV                => 5,
      TX_CLK25_DIV                => 5,
      UCODEER_CLR                 => '0',
      ---------------------------PCI Express Attributes----------------------------
      PCS_PCIE_EN                 => "FALSE",
      ---------------------------PCS Attributes----------------------------
      PCS_RSVD_ATTR               => X"000000000002",
      -------------RX Buffer Attributes------------
      RXBUF_ADDR_MODE             => "FAST",
      RXBUF_EIDLE_HI_CNT          => "1000",
      RXBUF_EIDLE_LO_CNT          => "0000",
      RXBUF_EN                    => "FALSE",
      RX_BUFFER_CFG               => "000000",
      RXBUF_RESET_ON_CB_CHANGE    => "TRUE",
      RXBUF_RESET_ON_COMMAALIGN   => "FALSE",
      RXBUF_RESET_ON_EIDLE        => "FALSE",
      RXBUF_RESET_ON_RATE_CHANGE  => "TRUE",
      RXBUFRESET_TIME             => "00001",
      RXBUF_THRESH_OVFLW          => 61,
      RXBUF_THRESH_OVRD           => "FALSE",
      RXBUF_THRESH_UNDFLW         => 4,
      RXDLY_CFG                   => X"001F",
      RXDLY_LCFG                  => X"030",
      RXDLY_TAP_CFG               => X"0000",
      RXPH_CFG                    => X"000000",
      RXPHDLY_CFG                 => X"084020",
      RXPH_MONITOR_SEL            => "00000",
      RX_XCLK_SEL                 => "RXUSR",
      RX_DDI_SEL                  => "000000",
      RX_DEFER_RESET_BUF_EN       => "TRUE",
      -----------------------CDR Attributes-------------------------
      --For Display Port, HBR/RBR- set RXCDR_CFG=72'h0380008bff40200008
      --For Display Port, HBR2 -   set RXCDR_CFG=72'h038c008bff20200010
      RXCDR_CFG                   => X"03000023ff20400020",
      RXCDR_FR_RESET_ON_EIDLE     => '0',
      RXCDR_HOLD_DURING_EIDLE     => '0',
      RXCDR_PH_RESET_ON_EIDLE     => '0',
      RXCDR_LOCK_CFG              => "010101",
      -------------------RX Initialization and Reset Attributes-------------------
      RXCDRFREQRESET_TIME         => "00001",
      RXCDRPHRESET_TIME           => "00001",
      RXISCANRESET_TIME           => "00001",
      RXPCSRESET_TIME             => "00001",
      RXPMARESET_TIME             => "00011",
      -------------------RX OOB Signaling Attributes-------------------
      RXOOB_CFG                   => "0000110",
      -------------------------RX Gearbox Attributes---------------------------
      RXGEARBOX_EN                => "FALSE",
      GEARBOX_MODE                => "000",
      -------------------------PRBS Detection Attribute-----------------------
      RXPRBS_ERR_LOOPBACK         => '0',
      -------------Power-Down Attributes----------
      PD_TRANS_TIME_FROM_P2       => X"03c",
      PD_TRANS_TIME_NONE_P2       => X"3c",
      PD_TRANS_TIME_TO_P2         => X"64",
      -------------RX OOB Signaling Attributes----------
      SAS_MAX_COM                 => 64,
      SAS_MIN_COM                 => 36,
      SATA_BURST_SEQ_LEN          => "1111",
      SATA_BURST_VAL              => "100",
      SATA_EIDLE_VAL              => "100",
      SATA_MAX_BURST              => 8,
      SATA_MAX_INIT               => 21,
      SATA_MAX_WAKE               => 7,
      SATA_MIN_BURST              => 4,
      SATA_MIN_INIT               => 12,
      SATA_MIN_WAKE               => 4,
      -------------RX Fabric Clock Output Control Attributes----------
      TRANS_TIME_RATE             => X"0E",
      --------------TX Buffer Attributes----------------
      TXBUF_EN                    => "TRUE",
      TXBUF_RESET_ON_RATE_CHANGE  => "TRUE",
      TXDLY_CFG                   => X"001F",
      TXDLY_LCFG                  => X"030",
      TXDLY_TAP_CFG               => X"0000",
      TXPH_CFG                    => X"0780",
      TXPHDLY_CFG                 => X"084020",
      TXPH_MONITOR_SEL            => "00000",
      TX_XCLK_SEL                 => "TXOUT",
      -------------------------FPGA TX Interface Attributes-------------------------
      TX_DATA_WIDTH               => 20,
      -------------------------TX Configurable Driver Attributes-------------------------
      TX_DEEMPH0                  => "00000",
      TX_DEEMPH1                  => "00000",
      TX_EIDLE_ASSERT_DELAY       => "110",
      TX_EIDLE_DEASSERT_DELAY     => "100",
      TX_LOOPBACK_DRIVE_HIZ       => "FALSE",
      TX_MAINCURSOR_SEL           => '0',
      TX_DRIVE_MODE               => "DIRECT",
      TX_MARGIN_FULL_0            => "1001110",
      TX_MARGIN_FULL_1            => "1001001",
      TX_MARGIN_FULL_2            => "1000101",
      TX_MARGIN_FULL_3            => "1000010",
      TX_MARGIN_FULL_4            => "1000000",
      TX_MARGIN_LOW_0             => "1000110",
      TX_MARGIN_LOW_1             => "1000100",
      TX_MARGIN_LOW_2             => "1000010",
      TX_MARGIN_LOW_3             => "1000000",
      TX_MARGIN_LOW_4             => "1000000",
      -------------------------TX Gearbox Attributes--------------------------
      TXGEARBOX_EN                => "FALSE",
      -------------------------TX Initialization and Reset Attributes--------------------------
      TXPCSRESET_TIME             => "00001",
      TXPMARESET_TIME             => "00001",
      -------------------------TX Receiver Detection Attributes--------------------------
      TX_RXDETECT_CFG             => X"1832",
      TX_RXDETECT_REF             => "100",
      ----------------------------CPLL Attributes----------------------------
      CPLL_CFG                    => X"BC07DC",
      CPLL_FBDIV                  => CPLL_FBDIV,
      CPLL_FBDIV_45               => CPLL_FBDIV_45,
      CPLL_INIT_CFG               => X"00001E",
      CPLL_LOCK_CFG               => X"01E8",
      CPLL_REFCLK_DIV             => CPLL_REFCLK_DIV,
      RXOUT_DIV                   => RXOUT_DIV,
      TXOUT_DIV                   => TXOUT_DIV,
      SATA_CPLL_CFG               => "VCO_3000MHZ",
      --------------RX Initialization and Reset Attributes-------------
      RXDFELPMRESET_TIME          => "0001111",
      --------------RX Equalizer Attributes-------------
      RXLPM_HF_CFG                => "00000011110000",
      RXLPM_LF_CFG                => "00000011110000",
      RX_DFE_GAIN_CFG             => X"020FEA",
      RX_DFE_H2_CFG               => "000000000000",
      RX_DFE_H3_CFG               => "000001000000",
      RX_DFE_H4_CFG               => "00011110000",
      RX_DFE_H5_CFG               => "00011100000",
      RX_DFE_KL_CFG               => "0000011111110",
      RX_DFE_LPM_CFG              => X"0954",
      RX_DFE_LPM_HOLD_DURING_EIDLE=> '0',
      RX_DFE_UT_CFG               => "10001111000000000",
      RX_DFE_VP_CFG               => "00011111100000011",
      -------------------------Power-Down Attributes-------------------------
      RX_CLKMUX_PD                => '1',
      TX_CLKMUX_PD                => '1',
      -------------------------FPGA RX Interface Attribute-------------------------
      RX_INT_DATAWIDTH            => 0,
      -------------------------FPGA TX Interface Attribute-------------------------
      TX_INT_DATAWIDTH            => 0,
      ------------------TX Configurable Driver Attributes---------------
      TX_QPI_STATUS_EN            => '0',
      -------------------------RX Equalizer Attributes--------------------------
      RX_DFE_KL_CFG2              => X"301148AC",
      RX_DFE_XYD_CFG              => "0000000000000",
      -------------------------TX Configurable Driver Attributes--------------------------
      TX_PREDRIVER_MODE           => '0'
   )
   port map
   (
      --------------------------------- CPLL Ports -------------------------------
      CPLLFBCLKLOST               => open,
      CPLLLOCKDETCLK              => i_gtx_ctrl.clk,
      CPLLLOCK                    => cplllock,
      CPLLLOCKEN                  => '1',
      CPLLPD                      => cpllpd,
      CPLLREFCLKLOST              => open,
      CPLLREFCLKSEL               => "001",
      CPLLRESET                   => cpllreset,
      GTRSVD                      => "0000000000000000",
      PCSRSVDIN                   => "0000000000000000",
      PCSRSVDIN2                  => "00000",
      PMARSVDIN                   => "00000",
      PMARSVDIN2                  => "00000",
      TSTIN                       => "11111111111111111111",
      TSTOUT                      => open,
      ---------------------------------- Channel ---------------------------------
      CLKRSVD                     => "0000",
      -------------------------- Channel - Clocking Ports ------------------------
      GTGREFCLK                   => '0',
      GTNORTHREFCLK0              => '0',
      GTNORTHREFCLK1              => '0',
      GTREFCLK0                   => i_gtx_ctrl.gtrefclk0,
      GTREFCLK1                   => i_gtx_ctrl.gtrefclk1,
      GTSOUTHREFCLK0              => '0',
      GTSOUTHREFCLK1              => '0',
      ---------------------------- Channel - DRP Ports  --------------------------
      DRPADDR                     => "000000000",
      DRPCLK                      => '0',
      DRPDI                       => X"0000",
      DRPDO                       => open,
      DRPEN                       => '0',
      DRPRDY                      => open,
      DRPWE                       => '0',
      ------------------------------- Clocking Ports -----------------------------
      GTREFCLKMONITOR             => open,
      QPLLCLK                     => i_gtx_ctrl.qpllclk,
      QPLLREFCLK                  => i_gtx_ctrl.qpllrefclk,
      RXSYSCLKSEL                 => "00",
      TXSYSCLKSEL                 => "00",
      --------------------------- Digital Monitor Ports --------------------------
      DMONITOROUT                 => open,
      ----------------- FPGA TX Interface Datapath Configuration  ----------------
      TX8B10BEN                   => '1',
      ------------------------------- Loopback Ports -----------------------------
      LOOPBACK                    => "000",
      ----------------------------- PCI Express Ports ----------------------------
      PHYSTATUS                   => open,
      RXRATE                      => "000",
      RXVALID                     => open,
      ------------------------------ Power-Down Ports ----------------------------
      RXPD                        => i_gtx.rx.rxpd,
      TXPD                        => i_gtx.tx.txpd,
      -------------------------- RX 8B/10B Decoder Ports -------------------------
      SETERRSTATUS                => '0',
      --------------------- RX Initialization and Reset Ports --------------------
      EYESCANRESET                => '0',
      RXUSERRDY                   => rxuserrdy,
      -------------------------- RX Margin Analysis Ports ------------------------
      EYESCANDATAERROR            => open,
      EYESCANMODE                 => '0',
      EYESCANTRIGGER              => '0',
      ------------------------- Receive Ports - CDR Ports ------------------------
      RXCDRFREQRESET              => '0',
      RXCDRHOLD                   => '0',
      RXCDRLOCK                   => open,
      RXCDROVRDEN                 => '0',
      RXCDRRESET                  => '0',
      RXCDRRESETRSV               => '0',
      ------------------- Receive Ports - Clock Correction Ports -----------------
      RXCLKCORCNT                 => open,
      ---------- Receive Ports - FPGA RX Interface Datapath Configuration --------
      RX8B10BEN                   => '1',
      ------------------ Receive Ports - FPGA RX Interface Ports -----------------
      RXUSRCLK                    => rxusrclk,
      RXUSRCLK2                   => rxusrclk,
      ------------------ Receive Ports - FPGA RX interface Ports -----------------
      RXDATA                      => rxdata,
      ------------------- Receive Ports - Pattern Checker Ports ------------------
      RXPRBSERR                   => open,
      RXPRBSSEL                   => "000",
      ------------------- Receive Ports - Pattern Checker ports ------------------
      RXPRBSCNTRESET              => '0',
      -------------------- Receive Ports - RX  Equalizer Ports -------------------
      RXDFEXYDEN                  => '1',
      RXDFEXYDHOLD                => '0',
      RXDFEXYDOVRDEN              => '0',
      ------------------ Receive Ports - RX 8B/10B Decoder Ports -----------------
      RXDISPERR                   => rxdisperr,
      RXNOTINTABLE                => rxnotintable,
      --------------------------- Receive Ports - RX AFE -------------------------
      GTXRXP                      => i_gtx.rx.gtxrxp,
      GTXRXN                      => i_gtx.rx.gtxrxn,
      ------------------- Receive Ports - RX Buffer Bypass Ports -----------------
      RXBUFRESET                  => '0',
      RXBUFSTATUS                 => open,
      RXDDIEN                     => '1',
      RXDLYBYPASS                 => '0',
      RXDLYEN                     => '0',
      RXDLYOVRDEN                 => '0',
      RXDLYSRESET                 => rxdlysreset,
      RXDLYSRESETDONE             => rxdlysresetdone,
      RXPHALIGN                   => '0',
      RXPHALIGNDONE               => rxphaligndone,
      RXPHALIGNEN                 => '0',
      RXPHDLYPD                   => '0',
      RXPHDLYRESET                => '0',
      RXPHMONITOR                 => open,
      RXPHOVRDEN                  => '0',
      RXPHSLIPMONITOR             => open,
      RXSTATUS                    => open,
      -------------- Receive Ports - RX Byte and Word Alignment Ports ------------
      RXBYTEISALIGNED             => rxbyteisaligned,
      RXBYTEREALIGN               => rxbyterealign,
      RXCOMMADET                  => rxcommadet,
      RXCOMMADETEN                => '1',
      RXMCOMMAALIGNEN             => '1',
      RXPCOMMAALIGNEN             => '1',
      ------------------ Receive Ports - RX Channel Bonding Ports ----------------
      RXCHANBONDSEQ               => open,
      RXCHBONDEN                  => '0',
      RXCHBONDLEVEL               => "000",
      RXCHBONDMASTER              => '0',
      RXCHBONDO                   => open,
      RXCHBONDSLAVE               => '0',
      ----------------- Receive Ports - RX Channel Bonding Ports  ----------------
      RXCHANISALIGNED             => open,
      RXCHANREALIGN               => open,
      -------------------- Receive Ports - RX Equalizer Ports --------------------
      RXLPMHFHOLD                 => rxlpmhfhold,
      RXLPMHFOVRDEN               => '0',
      RXLPMLFHOLD                 => rxlpmlfhold,
      --------------------- Receive Ports - RX Equalizer Ports -------------------
      RXDFEAGCHOLD                => rxdfeagchold,
      RXDFEAGCOVRDEN              => '0',
      RXDFECM1EN                  => '0',
      RXDFELFHOLD                 => rxdfelfhold,
      RXDFELFOVRDEN               => '1',
      RXDFELPMRESET               => '0',
      RXDFETAP2HOLD               => '0',
      RXDFETAP2OVRDEN             => '0',
      RXDFETAP3HOLD               => '0',
      RXDFETAP3OVRDEN             => '0',
      RXDFETAP4HOLD               => '0',
      RXDFETAP4OVRDEN             => '0',
      RXDFETAP5HOLD               => '0',
      RXDFETAP5OVRDEN             => '0',
      RXDFEUTHOLD                 => '0',
      RXDFEUTOVRDEN               => '0',
      RXDFEVPHOLD                 => '0',
      RXDFEVPOVRDEN               => '0',
      RXDFEVSEN                   => '0',
      RXLPMLFKLOVRDEN             => '0',
      RXMONITOROUT                => open,
      RXMONITORSEL                => "00",
      RXOSHOLD                    => '0',
      RXOSOVRDEN                  => '0',
      ------------ Receive Ports - RX Fabric ClocK Output Control Ports ----------
      RXRATEDONE                  => open,
      --------------- Receive Ports - RX Fabric Output Control Ports -------------
      RXOUTCLK                    => rxoutclk,
      RXOUTCLKFABRIC              => open,
      RXOUTCLKPCS                 => open,
      RXOUTCLKSEL                 => "010",
      ---------------------- Receive Ports - RX Gearbox Ports --------------------
      RXDATAVALID                 => open,
      RXHEADER                    => open,
      RXHEADERVALID               => open,
      RXSTARTOFSEQ                => open,
      --------------------- Receive Ports - RX Gearbox Ports  --------------------
      RXGEARBOXSLIP               => '0',
      ------------- Receive Ports - RX Initialization and Reset Ports ------------
      GTRXRESET                   => gtrxreset,
      RXOOBRESET                  => '0',
      RXPCSRESET                  => '0',
      RXPMARESET                  => '0',
      ------------------ Receive Ports - RX Margin Analysis ports ----------------
      RXLPMEN                     => '0',
      ------------------- Receive Ports - RX OOB Signaling ports -----------------
      RXCOMSASDET                 => open,
      RXCOMWAKEDET                => open,
      ------------------ Receive Ports - RX OOB Signaling ports  -----------------
      RXCOMINITDET                => open,
      ------------------ Receive Ports - RX OOB signalling Ports -----------------
      RXELECIDLE                  => open,
      RXELECIDLEMODE              => "11",
      ----------------- Receive Ports - RX Polarity Control Ports ----------------
      RXPOLARITY                  => '0',
      ---------------------- Receive Ports - RX gearbox ports --------------------
      RXSLIDE                     => '0',
      ------------------- Receive Ports - RX8B/10B Decoder Ports -----------------
      RXCHARISCOMMA               => open,
      RXCHARISK                   => rxcharisk,
      ------------------ Receive Ports - Rx Channel Bonding Ports ----------------
      RXCHBONDI                   => "00000",
      -------------- Receive Ports -RX Initialization and Reset Ports ------------
      RXRESETDONE                 => rxresetdone,
      -------------------------------- Rx AFE Ports ------------------------------
      RXQPIEN                     => '0',
      RXQPISENN                   => open,
      RXQPISENP                   => open,
      --------------------------- TX Buffer Bypass Ports -------------------------
      TXPHDLYTSTCLK               => '0',
      ------------------------ TX Configurable Driver Ports ----------------------
      TXPOSTCURSOR                => "00000",
      TXPOSTCURSORINV             => '0',
      TXPRECURSOR                 => "00000",
      TXPRECURSORINV              => '0',
      TXQPIBIASEN                 => '0',
      TXQPISTRONGPDOWN            => '0',
      TXQPIWEAKPUP                => '0',
      --------------------- TX Initialization and Reset Ports --------------------
      CFGRESET                    => '0',
      GTTXRESET                   => gttxreset,
      PCSRSVDOUT                  => open,
      TXUSERRDY                   => txuserrdy,
      ---------------------- Transceiver Reset Mode Operation --------------------
      GTRESETSEL                  => '0',
      RESETOVRD                   => '0',
      ---------------- Transmit Ports - 8b10b Encoder Control Ports --------------
      TXCHARDISPMODE              => X"00",
      TXCHARDISPVAL               => X"00",
      ------------------ Transmit Ports - FPGA TX Interface Ports ----------------
      TXUSRCLK                    => txusrclk,
      TXUSRCLK2                   => txusrclk,
      --------------------- Transmit Ports - PCI Express Ports -------------------
      TXELECIDLE                  => '0',
      TXMARGIN                    => "000",
      TXRATE                      => "000",
      TXSWING                     => '0',
      ------------------ Transmit Ports - Pattern Generator Ports ----------------
      TXPRBSFORCEERR              => '0',
      ------------------ Transmit Ports - TX Buffer Bypass Ports -----------------
      TXDLYBYPASS                 => '1',
      TXDLYEN                     => '0',
      TXDLYHOLD                   => '0',
      TXDLYOVRDEN                 => '0',
      TXDLYSRESET                 => '0',
      TXDLYSRESETDONE             => open,
      TXDLYUPDOWN                 => '0',
      TXPHALIGN                   => '0',
      TXPHALIGNDONE               => open,
      TXPHALIGNEN                 => '0',
      TXPHDLYPD                   => '0',
      TXPHDLYRESET                => '0',
      TXPHINIT                    => '0',
      TXPHINITDONE                => open,
      TXPHOVRDEN                  => '0',
      ---------------------- Transmit Ports - TX Buffer Ports --------------------
      TXBUFSTATUS                 => open,
      --------------- Transmit Ports - TX Configurable Driver Ports --------------
      TXBUFDIFFCTRL               => "100",
      TXDEEMPH                    => '0',
      TXDIFFCTRL                  => "1000",
      TXDIFFPD                    => '0',
      TXINHIBIT                   => '0',
      TXMAINCURSOR                => "0000000",
      TXPISOPD                    => '0',
      ------------------ Transmit Ports - TX Data Path interface -----------------
      TXDATA                      => txdata,
      ---------------- Transmit Ports - TX Driver and OOB signalling -------------
      GTXTXN                      => o_gtx.tx.gtxtxn,
      GTXTXP                      => o_gtx.tx.gtxtxp,
      ----------- Transmit Ports - TX Fabric Clock Output Control Ports ----------
      TXOUTCLK                    => txoutclk,
      TXOUTCLKFABRIC              => open,
      TXOUTCLKPCS                 => open,
      TXOUTCLKSEL                 => "010",
      TXRATEDONE                  => open,
      --------------------- Transmit Ports - TX Gearbox Ports --------------------
      TXCHARISK                   => txcharisk,
      TXGEARBOXREADY              => open,
      TXHEADER                    => "000",
      TXSEQUENCE                  => "0000000",
      TXSTARTSEQ                  => '0',
      ------------- Transmit Ports - TX Initialization and Reset Ports -----------
      TXPCSRESET                  => '0',
      TXPMARESET                  => '0',
      TXRESETDONE                 => txresetdone,
      ------------------ Transmit Ports - TX OOB signalling Ports ----------------
      TXCOMFINISH                 => open,
      TXCOMINIT                   => '0',
      TXCOMSAS                    => '0',
      TXCOMWAKE                   => '0',
      TXPDELECIDLEMODE            => '0',
      ----------------- Transmit Ports - TX Polarity Control Ports ---------------
      TXPOLARITY                  => '0',
      --------------- Transmit Ports - TX Receiver Detection Ports  --------------
      TXDETECTRX                  => '0',
      ------------------ Transmit Ports - TX8b/10b Encoder Ports -----------------
      TX8B10BBYPASS               => X"00",
      ------------------ Transmit Ports - pattern Generator Ports ----------------
      TXPRBSSEL                   => "000",
      ----------------------- Tx Configurable Driver  Ports ----------------------
      TXQPISENN                   => open,
      TXQPISENP                   => open
   );

 end structural;

--------------------------------------------------------------------------------
-- End of file
--------------------------------------------------------------------------------
