--------------------------------------------------------------------------------
--                       Paul Scherrer Institute (PSI)
--------------------------------------------------------------------------------
-- Unit    : gtx_ipif.vhd
-- Author  : Goran Marinkovic, Section Diagnostic
-- Version : $Revision: 1.6 $
--------------------------------------------------------------------------------
-- Copyright© PSI, Section Diagnostic
--------------------------------------------------------------------------------
-- Comment : This is the frame package for the GTXE2 component.
--------------------------------------------------------------------------------
-- Std. library (platform) -----------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

package gtx_ipif_package is

   -----------------------------------------------------------------------------
   -- RX comma detect
   -----------------------------------------------------------------------------
   component rx_comma_detect is
   generic
   (
      LINK_DET_THRESH             : std_logic_vector( 7 downto  0) := X"20"; -- Number of consecutive commas of same kind needed to achieve lock
      K_IDLE_PLB                  : std_logic_vector( 7 downto  0) := X"BC"; -- K28.5
      K_IDLE_AXI                  : std_logic_vector( 7 downto  0) := X"3C"  -- K28.1
   );
   port
   (
      rxusrclk                    : in    std_logic;
      rxdisperr                   : in    std_logic_vector( 1 downto 0);
      rxnotintable                : in    std_logic_vector( 1 downto 0);
      rxcharisk                   : in    std_logic_vector( 1 downto 0);
      rxdata                      : in    std_logic_vector(15 downto 0);
      --lock info
      comma_lock_axi              : out   std_logic;
      comma_lock_plb              : out   std_logic;
      comma_lock                  : out   std_logic
   );
   end component rx_comma_detect;

   -----------------------------------------------------------------------------
   -- RX AXI FIFO
   -----------------------------------------------------------------------------
   component rx_gtx_fifo is
   generic
   (
      K_IDLE_PLB                  : std_logic_vector( 7 downto  0) := X"BC"; -- K28.5
      K_IDLE_AXI                  : std_logic_vector( 7 downto  0) := X"3C"; -- K28.1
      K_SOF                       : std_logic_vector( 7 downto  0) := X"FB"; -- K27.7
      K_EOF                       : std_logic_vector( 7 downto  0) := X"FD"; -- K29.7
      K_INT                       : std_logic_vector( 7 downto  0) := X"DC"; -- K28.6
      K_XON                       : std_logic_vector( 7 downto  0) := X"1C"; -- K28.0
      K_XOFF                      : std_logic_vector( 7 downto  0) := X"5C"  -- K28.2
   );
   port
   (
      --------------------------------------------------------------------------
      -- Debug
      --------------------------------------------------------------------------
      debug_clk                   : out    std_logic;
      debug                       : out    std_logic_vector(127 downto  0);
      --------------------------------------------------------------------------
      -- GTX clock side
      --------------------------------------------------------------------------
      -- System
      rxusrclk                    : in    std_logic;
      -- GTX rx
      rxresetdone                 : in    std_logic;
      rxdisperr                   : in    std_logic_vector( 1 downto  0);
      rxnotintable                : in    std_logic_vector( 1 downto  0);
      rxcharisk                   : in    std_logic_vector( 1 downto  0);
      rxdata                      : in    std_logic_vector(15 downto  0);
      -- GTX tx flow control
      rx_xoff                     : out   std_logic := '0';
      tx_xoff                     : out   std_logic := '0';
      --------------------------------------------------------------------------
      -- AXI clock side
      --------------------------------------------------------------------------
      -- System
      aclk                        : in    std_logic;
      -- FIFO status
      axi_tready                  : in    std_logic;
      axi_tvalid                  : out   std_logic;
      axi_tdata                   : out   std_logic_vector(31 downto  0);
      axi_tuser                   : out   std_logic_vector( 3 downto  0)
   );
   end component rx_gtx_fifo;

   -----------------------------------------------------------------------------
   -- TX AXI FIFO
   -----------------------------------------------------------------------------
   component tx_gtx_fifo is
   generic
   (
      K_IDLE_PLB                  : std_logic_vector( 7 downto  0) := X"BC"; -- K28.5
      K_IDLE_AXI                  : std_logic_vector( 7 downto  0) := X"3C"; -- K28.1
      K_SOF                       : std_logic_vector( 7 downto  0) := X"FB"; -- K27.7
      K_EOF                       : std_logic_vector( 7 downto  0) := X"FD"; -- K29.7
      K_XON                       : std_logic_vector( 7 downto  0) := X"1C"; -- K28.0
      K_XOFF                      : std_logic_vector( 7 downto  0) := X"5C"  -- K28.2
   );
   port
   (
      --------------------------------------------------------------------------
      -- AXI clock side
      --------------------------------------------------------------------------
      -- System
      aclk                        : in    std_logic;
      -- AXI read interface
      axi_tready                  : out   std_logic;
      axi_tvalid                  : in    std_logic;
      axi_tdata                   : in    std_logic_vector(31 downto  0);
      axi_tuser                   : in    std_logic_vector( 3 downto  0);
      --------------------------------------------------------------------------
      -- GTX clock side
      --------------------------------------------------------------------------
      -- System
      txusrclk                    : in    std_logic;
      -- GTX tx flow control
      comma_lock_axi              : in    std_logic;
      rx_xoff                     : in    std_logic;
      tx_xoff                     : in    std_logic;
      -- GTX rx
      txresetdone                 : in    std_logic;
      txcharisk                   : out   std_logic_vector( 1 downto  0) := "01";
      txdata                      : out   std_logic_vector(15 downto  0) := X"00" & K_IDLE_AXI
   );
   end component tx_gtx_fifo;

end package gtx_ipif_package;

--------------------------------------------------------------------------------
-- RX comma detect
--------------------------------------------------------------------------------
-- Std. library (platform) -----------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Work library (platform) -----------------------------------------------------
library unisim;
use unisim.vcomponents.all;

entity rx_comma_detect is
   generic
   (
      LINK_DET_THRESH             : std_logic_vector( 7 downto  0) := X"20"; -- Number of consecutive commas of same kind needed to achieve lock
      K_IDLE_PLB                  : std_logic_vector( 7 downto  0) := X"BC"; -- K28.5
      K_IDLE_AXI                  : std_logic_vector( 7 downto  0) := X"3C"  -- K28.1
   );
   port
   (
      rxusrclk                    : in    std_logic;
      rxdisperr                   : in    std_logic_vector( 1 downto 0);
      rxnotintable                : in    std_logic_vector( 1 downto 0);
      rxcharisk                   : in    std_logic_vector( 1 downto 0);
      rxdata                      : in    std_logic_vector(15 downto 0);
      --lock info
      comma_lock_axi              : out   std_logic := '0';
      comma_lock_plb              : out   std_logic := '0';
      comma_lock                  : out   std_logic := '0'
   );
end entity rx_comma_detect;

architecture behavioral of rx_comma_detect is

   signal   rxerror               : std_logic := '0';
   signal   rxiscomma_axi         : std_logic := '0';
   signal   rxiscomma_plb         : std_logic := '0';
   signal   could_be_axi          : std_logic := '0';
   signal   could_be_plb          : std_logic := '0';
   signal   comma_cnt             : unsigned( 7 downto  0) := X"00";
   signal   is_axi                : std_logic := '0';
   signal   is_plb                : std_logic := '0';

begin

   -----------------------------------------------------------------------------
   -- Register data
   -----------------------------------------------------------------------------
   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         if ((rxdisperr = "00") and (rxnotintable = "00")) then
            rxerror               <= '0';
         else
            rxerror               <= '1';
         end if;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- Detect comma
   -----------------------------------------------------------------------------
   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         if ((rxcharisk = "01") and (rxdata( 7 downto  0) = K_IDLE_AXI) and (rxerror = '0')) then
            rxiscomma_axi         <= '1';
         else
            rxiscomma_axi         <= '0';
         end if;
      end if;
   end process;

   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         if ((rxcharisk = "01") and (rxdata( 7 downto  0) = K_IDLE_PLB) and (rxerror = '0')) then
            rxiscomma_plb         <= '1';
         else
            rxiscomma_plb         <= '0';
         end if;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- Assume PLB or AXI bus
   -----------------------------------------------------------------------------
   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         if (rxerror = '1') then
            could_be_axi          <= '0';
         else
            if (rxiscomma_plb = '1') then
               could_be_axi       <= '0';
            else
               if (rxiscomma_axi = '1') then
                  could_be_axi    <= '1';
               end if;
            end if;
         end if;
      end if;
   end process;

   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         if (rxerror = '1') then
            could_be_plb          <= '0';
         else
            if (rxiscomma_axi = '1') then
               could_be_plb       <= '0';
            else
               if (rxiscomma_plb = '1') then
                  could_be_plb    <= '1';
               end if;
            end if;
         end if;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- Assure PLB or AXI bus over several commas
   -----------------------------------------------------------------------------
   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         if    (((rxiscomma_plb = '1') and (could_be_plb = '1')) or
                ((rxiscomma_axi = '1') and (could_be_axi = '1'))) then
            if (comma_cnt /= unsigned(LINK_DET_THRESH)) then
               comma_cnt          <= comma_cnt + X"01";
            end if;
         elsif (((rxiscomma_plb = '1') and (could_be_axi = '1')) or
                ((rxiscomma_axi = '1') and (could_be_plb = '1'))) then
            comma_cnt             <= X"00";
         end if;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- Founs PLB or AXI bus
   -----------------------------------------------------------------------------
   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         if ((could_be_axi = '1') and (comma_cnt = unsigned(LINK_DET_THRESH))) then
            is_axi                <= '1';
         else
            is_axi                <= '0';
         end if;
      end if;
   end process;

   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         if ((could_be_plb = '1') and (comma_cnt = unsigned(LINK_DET_THRESH))) then
            is_plb                <= '1';
         else
            is_plb                <= '0';
         end if;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- Output sync
   -----------------------------------------------------------------------------
   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         comma_lock_plb           <= is_plb;
         comma_lock_axi           <= is_axi;
         comma_lock               <= is_axi or is_plb;
      end if;
   end process;

end architecture behavioral;

--------------------------------------------------------------------------------
-- RX AXI FIFO
--------------------------------------------------------------------------------
-- Std. library (platform) -----------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Work library (platform) -----------------------------------------------------
library unisim;
use unisim.vcomponents.all;

-- Work library (application) --------------------------------------------------
use work.gtx_link_package.all;

entity rx_gtx_fifo is
   generic
   (
      K_IDLE_PLB                  : std_logic_vector( 7 downto  0) := X"BC"; -- K28.5
      K_IDLE_AXI                  : std_logic_vector( 7 downto  0) := X"3C"; -- K28.1
      K_SOF                       : std_logic_vector( 7 downto  0) := X"FB"; -- K27.7
      K_EOF                       : std_logic_vector( 7 downto  0) := X"FD"; -- K29.7
      K_INT                       : std_logic_vector( 7 downto  0) := X"DC"; -- K28.6
      K_XON                       : std_logic_vector( 7 downto  0) := X"1C"; -- K28.0
      K_XOFF                      : std_logic_vector( 7 downto  0) := X"5C"  -- K28.2
   );
   port
   (
      --------------------------------------------------------------------------
      -- Debug
      --------------------------------------------------------------------------
      debug_clk                   : out    std_logic;
      debug                       : out    std_logic_vector(127 downto  0);
      --------------------------------------------------------------------------
      -- GTX clock side
      --------------------------------------------------------------------------
      -- System
      rxusrclk                    : in    std_logic;
      -- GTX rx
      rxresetdone                 : in    std_logic;
      rxdisperr                   : in    std_logic_vector( 1 downto  0);
      rxnotintable                : in    std_logic_vector( 1 downto  0);
      rxcharisk                   : in    std_logic_vector( 1 downto  0);
      rxdata                      : in    std_logic_vector(15 downto  0);
      -- GTX tx flow control
      rx_xoff                     : out   std_logic := '0';
      tx_xoff                     : out   std_logic := '0';
      --------------------------------------------------------------------------
      -- AXI clock side
      --------------------------------------------------------------------------
      -- System
      aclk                        : in    std_logic;
      -- FIFO status
      axi_tready                  : in    std_logic;
      axi_tvalid                  : out   std_logic;
      axi_tdata                   : out   std_logic_vector(31 downto  0);
      axi_tuser                   : out   std_logic_vector( 3 downto  0)
   );
end entity rx_gtx_fifo;

architecture structural of rx_gtx_fifo is

   signal   rxresetdone_sync      : std_logic := '0';
   signal   rxerror               : std_logic := '0';
   signal   rxcharisk_r           : std_logic_vector( 1 downto 0) := (others => '0');
   signal   rxdata_r              : std_logic_vector(15 downto 0) := (others => '0');
   signal   rx_is_xon             : std_logic := '0';
   signal   rx_is_xoff            : std_logic := '0';

   type state_type is
   (
      get_word_hi,
      get_word_lo
   );

   signal   state                 : state_type := get_word_lo;

   signal   fifo_rst              : std_logic_vector( 4 downto 0) := (others => '1');
   signal   fifo_amostfull        : std_logic := '0';
   signal   fifo_wren             : std_logic := '0';
   signal   fifo_di               : std_logic_vector(31 downto 0) := (others => '0');
   signal   fifo_dip              : std_logic_vector( 3 downto 0) := (others => '0');

   signal   fifo_empty            : std_logic := '0';
   signal   fifo_rden             : std_logic := '0';
   signal   fifo_do               : std_logic_vector(31 downto 0) := (others => '0');
   signal   fifo_dop              : std_logic_vector( 3 downto 0) := (others => '0');

   signal   rxerror_v             : std_logic_vector(1 downto 0);

begin

   -----------------------------------------------------------------------------
   -- Debug
   -----------------------------------------------------------------------------
   debug_clk                      <= rxusrclk;
   debug(  0)                     <= rxerror;
   debug(  2 downto  1)           <= rxcharisk_r;
   debug(  4 downto  3)           <= rxerror_v;
   debug(  7 downto  5)           <= "000";
   debug(  8)                     <= '0' when (state = get_word_hi) else '1';
   debug( 15 downto  9)           <= "0000000";
   debug( 31 downto 16)           <= rxdata_r;
   debug( 32)                     <= fifo_wren;
   debug( 36 downto 33)           <= fifo_dip;
   debug( 47 downto 37)           <= "00000000000";
   debug( 63 downto 48)           <= X"0000";
   debug( 95 downto 64)           <= fifo_di;
   debug(127 downto 96)           <= X"00000000";

   -----------------------------------------------------------------------------
   -- Reset is from the stable clock domain in the GTX and needs resynchronizing
   -----------------------------------------------------------------------------
   rxresetdone_gtx_sync_block_inst: gtx_sync_block
   port map
   (
      clk                         =>  rxusrclk,
      data_in                     =>  rxresetdone,
      data_out                    =>  rxresetdone_sync
   );

   -----------------------------------------------------------------------------
   -- Register data
   -----------------------------------------------------------------------------
   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         if ((rxdisperr = "00") and (rxnotintable = "00")) then
            rxerror               <= '0';
         else
            rxerror               <= '1';
         end if;
         rxcharisk_r              <= rxcharisk;
         rxdata_r                 <= rxdata;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- Tx flow control
   -----------------------------------------------------------------------------
   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         if ((rxcharisk_r = "01") and (rxdata_r( 7 downto  0) = K_XON) and (rxerror = '0')) then
            rx_is_xon             <= '1';
         else
            rx_is_xon             <= '0';
         end if;
      end if;
   end process;

   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         if ((rxcharisk_r = "01") and (rxdata_r( 7 downto  0) = K_XOFF) and (rxerror = '0')) then
            rx_is_xoff            <= '1';
         else
            rx_is_xoff            <= '0';
         end if;
      end if;
   end process;

   --DEBUG: Test disable XOFF
   ---process(rxusrclk)
   --begin
  --    if (rxusrclk'event and (rxusrclk = '1')) then
  --       if (rx_is_xon = '1') then
            rx_xoff               <= '0';
  --       elsif (rx_is_xoff = '1') then
  --          rx_xoff               <= '1';
  --       end if;
  --    end if;
  -- end process;
--
--   tx_xoff                        <= fifo_amostfull;
    tx_xoff <= '0';

   -----------------------------------------------------------------------------
   -- FIFO reset
   -- The Xilinx user guide states:
   --   RST must be held high for 3 WRCLK cycles prior to operation and any
   --   subsequent reset.
   -----------------------------------------------------------------------------
   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         case (fifo_rst) is
         when "00000" =>
            if (rxresetdone_sync = '0') then
               fifo_rst    <= "00001";
            end if;
         when "11111" =>
            if (rxresetdone_sync = '1') then
               fifo_rst    <= "11110";
            end if;
         when others =>
            fifo_rst       <= fifo_rst( 3 downto  0) & fifo_rst( 0);
         end case;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- FIFO write FSM
   -- The Xilinx user guide states:
   --    WREN must be held Low for four clock cycles before Reset is asserted
   --    and remain Low during the Reset cycle.
   -----------------------------------------------------------------------------
   process(rxusrclk)
   begin
      if (rxusrclk'event and (rxusrclk = '1')) then
         fifo_di                  <= rxdata_r & fifo_di(31 downto 16);
         fifo_dip                 <= rxcharisk_r & fifo_dip( 3 downto  2);
         rxerror_v                <= rxerror & rxerror_v(1); --ML84 added
         case (state) is
         when get_word_hi =>
             --if ((fifo_rst = "00000") and (rxerror = '0')) then 
             if ((fifo_rst = "00000") and (rxerror_v = "00")) then --ML84: discard whole dwords when even a single byte is corrupted
               if (((rxcharisk_r = "00")                                                ) or
                   ((fifo_dip( 3 downto  2) = "01") and (fifo_di (23 downto 16) = K_INT)) or
                   ((rxcharisk_r = "01")            and (rxdata_r( 7 downto  0) = K_SOF))) then

                  fifo_wren       <= '1';
                  state           <= get_word_lo;
               elsif ((fifo_dip( 3 downto  2) = "01") and (fifo_di (23 downto 16) = K_EOF)) then
                  fifo_wren       <= '1';
               else
                  fifo_wren       <= '0';
               end if;
            else
               fifo_wren          <= '0';
            end if;
         when get_word_lo =>
            fifo_wren             <= '0';
            state                 <= get_word_hi;
         when others =>
            fifo_wren             <= '0';
            state                 <= get_word_hi;
         end case;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- FIFO
   -----------------------------------------------------------------------------
   fifo18e1_inst: FIFO18E1
   generic map
   (
      SIM_DEVICE                  => "7SERIES",
      FIFO_MODE                   => "FIFO18_36",
      DATA_WIDTH                  => 36,
      FIRST_WORD_FALL_THROUGH     => TRUE,
      ALMOST_FULL_OFFSET          => X"0080",
      ALMOST_EMPTY_OFFSET         => X"0080",
      EN_SYN                      => FALSE,
      DO_REG                      => 1,
      INIT                        => X"000000000",
      SRVAL                       => X"000000000"
   )
   port map
   (
      RST                         => fifo_rst( 4),
      WRCLK                       => rxusrclk,
      WREN                        => fifo_wren,
      DI                          => fifo_di,
      DIP                         => fifo_dip,
      WRERR                       => open,
      WRCOUNT                     => open,
      ALMOSTFULL                  => fifo_amostfull,
      FULL                        => open,

      RSTREG                      => '0',
      REGCE                       => '1',
      RDCLK                       => aclk,
      RDEN                        => fifo_rden,
      DO                          => fifo_do,
      DOP                         => fifo_dop,
      RDERR                       => open,
      RDCOUNT                     => open,
      ALMOSTEMPTY                 => open,
      EMPTY                       => fifo_empty
   );

   -----------------------------------------------------------------------------
   -- FIFO read
   -----------------------------------------------------------------------------
   axi_tvalid                     <= '1'        when (fifo_empty = '0') else '0';
   fifo_rden                      <= axi_tready when (fifo_empty = '0') else '0';
   axi_tdata                      <= fifo_do;
   axi_tuser                      <= fifo_dop;

end architecture structural;

--------------------------------------------------------------------------------
-- TX AXI FIFO
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

entity tx_gtx_fifo is
   generic
   (
      K_IDLE_PLB                  : std_logic_vector( 7 downto  0) := X"BC"; -- K28.5
      K_IDLE_AXI                  : std_logic_vector( 7 downto  0) := X"3C"; -- K28.1
      K_SOF                       : std_logic_vector( 7 downto  0) := X"FB"; -- K27.7
      K_EOF                       : std_logic_vector( 7 downto  0) := X"FD"; -- K29.7
      K_XON                       : std_logic_vector( 7 downto  0) := X"1C"; -- K28.0
      K_XOFF                      : std_logic_vector( 7 downto  0) := X"5C"  -- K28.2
   );
   port
   (
      --------------------------------------------------------------------------
      -- AXI clock side
      --------------------------------------------------------------------------
      -- System
      aclk                        : in    std_logic;
      -- AXI read interface
      axi_tready                  : out   std_logic;
      axi_tvalid                  : in    std_logic;
      axi_tdata                   : in    std_logic_vector(31 downto  0);
      axi_tuser                   : in    std_logic_vector( 3 downto  0);
      --------------------------------------------------------------------------
      -- GTX clock side
      --------------------------------------------------------------------------
      -- System
      txusrclk                    : in    std_logic;
      -- GTX tx flow control
      comma_lock_axi              : in    std_logic;
      rx_xoff                     : in    std_logic;
      tx_xoff                     : in    std_logic;
      -- GTX rx
      txresetdone                 : in    std_logic;
      txcharisk                   : out   std_logic_vector( 1 downto  0) := "01";
      txdata                      : out   std_logic_vector(15 downto  0) := X"00" & K_IDLE_AXI
   );
end entity tx_gtx_fifo;

architecture structural of tx_gtx_fifo is

   signal   txresetdone_sync      : std_logic := '0';
   signal   fifo_rst              : std_logic_vector( 4 downto 0) := (others => '0');
   signal   fifo_full             : std_logic := '0';
   signal   fifo_wren             : std_logic := '0';
   signal   fifo_di               : std_logic_vector(31 downto 0) := (others => '0');
   signal   fifo_dip              : std_logic_vector( 3 downto 0) := (others => '0');

   signal   fifo_rden             : std_logic := '0';
   signal   fifo_do               : std_logic_vector(31 downto 0) := (others => '0');
   signal   fifo_dop              : std_logic_vector( 3 downto 0) := (others => '0');
   signal   fifo_empty            : std_logic := '0';

   type state_type is
   (
      idle,
      send_word_lo,
      send_word_hi,
      send_xon,
      send_xoff
   );

   signal   state                 : state_type := idle;

   signal   txdata_r              : std_logic_vector(15 downto 0) := (others => '0');
   signal   txcharisk_r           : std_logic_vector( 1 downto 0) := (others => '0');

   signal   rx_is_xon             : std_logic := '0';
   signal   rx_is_xoff            : std_logic := '0';

   signal   comma_lock_axi_sync   : std_logic := '0';
   signal   k_idle                : std_logic_vector( 7 downto 0) := K_IDLE_AXI;
   signal   rx_xoff_sync          : std_logic := '0';
   signal   tx_xoff_sync          : std_logic := '0';
   constant TX_XOFF_CYCLES        : integer := 125000;
   signal   tx_xoff_timer         : integer range 0 to TX_XOFF_CYCLES := TX_XOFF_CYCLES;

begin

   -----------------------------------------------------------------------------
   -- Reset is from the stable clock domain in the GTX and needs resynchronizing
   -----------------------------------------------------------------------------
   txresetdone_gtx_sync_block_inst: entity axis_gtx_bridge_v1_0_lib.gtx_sync_block
   port map
   (
      clk                         =>  aclk,
      data_in                     =>  txresetdone,
      data_out                    =>  txresetdone_sync
   );

   -----------------------------------------------------------------------------
   -- FIFO reset
   -- The Xilinx user guide states:
   --   RST must be held high for 3 WRCLK cycles prior to operation and any
   --   subsequent reset.
   -----------------------------------------------------------------------------
   process(aclk)
   begin
      if rising_edge(aclk) then
         case (fifo_rst) is
         when "00000" =>
            if (txresetdone_sync = '0') then
               fifo_rst           <= "00001";
            end if;
         when "11111" =>
            if (txresetdone_sync = '1') then
               fifo_rst           <= "11110";
            end if;
         when others =>
            fifo_rst              <= fifo_rst( 3 downto  0) & fifo_rst( 0);
         end case;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- FIFO write
   -- The Xilinx user guide states:
   --    WREN must be held Low for four clock cycles before Reset is asserted
   --    and remain Low during the Reset cycle.
   -----------------------------------------------------------------------------
   axi_tready                     <= '1'        when (fifo_full = '0') else '0';
   fifo_wren                      <= axi_tvalid when (fifo_full = '0') and (fifo_rst = "00000") else '0';
   fifo_di                        <= axi_tdata;
   fifo_dip                       <= axi_tuser;

   -----------------------------------------------------------------------------
   -- FIFO
   -----------------------------------------------------------------------------
   fifo18e1_inst: FIFO18E1
   generic map
   (
      SIM_DEVICE                  => "7SERIES",
      FIFO_MODE                   => "FIFO18_36",
      DATA_WIDTH                  => 36,
      FIRST_WORD_FALL_THROUGH     => TRUE,
      ALMOST_FULL_OFFSET          => X"0080",
      ALMOST_EMPTY_OFFSET         => X"0080",
      EN_SYN                      => FALSE,
      DO_REG                      => 1,
      INIT                        => X"000000000",
      SRVAL                       => X"000000000"
   )
   port map
   (
      RST                         => fifo_rst( 4),
      WRCLK                       => aclk,
      WREN                        => fifo_wren,
      DI                          => fifo_di,
      DIP                         => fifo_dip,
      WRERR                       => open,
      WRCOUNT                     => open,
      ALMOSTFULL                  => open,
      FULL                        => fifo_full,

      RSTREG                      => '0',
      REGCE                       => '1',
      RDCLK                       => txusrclk,
      RDEN                        => fifo_rden,
      DO                          => fifo_do,
      DOP                         => fifo_dop,
      RDERR                       => open,
      RDCOUNT                     => open,
      ALMOSTEMPTY                 => open,
      EMPTY                       => fifo_empty
   );

   -----------------------------------------------------------------------------
   -- FIFO read FSM
   -----------------------------------------------------------------------------
   process(txusrclk)
   begin
      if (txusrclk'event and (txusrclk = '1')) then
         case (state) is
         when idle =>
            if (tx_xoff_timer = 0) then
               if (tx_xoff_sync = '0') then
                  state           <= send_xon;
               else
                  state           <= send_xoff;
               end if;
            else
               if ((fifo_empty = '0') and (rx_xoff_sync = '0')) then
                  state           <= send_word_lo;
               end if;
            end if;
         when send_word_lo =>
            state                 <= send_word_hi;
         when send_word_hi =>
            if ((fifo_empty = '0') and (rx_xoff_sync = '0')) then
               state              <= send_word_lo;
            else
               state              <= idle;
            end if;
         when send_xon =>
            state                 <= idle;
         when send_xoff =>
            state                 <= idle;
         when others =>
            state                 <= idle;
         end case;
      end if;
   end process;

   -----------------------------------------------------------------------------
   -- FIFO read
   -----------------------------------------------------------------------------
   process(txusrclk)
   begin
      if (txusrclk'event and (txusrclk = '1')) then
         case (state) is
         when send_word_lo =>
            txdata                <= fifo_do(15 downto  0);
            txcharisk             <= fifo_dop( 1 downto  0);
            txdata_r              <= fifo_do(31 downto 16);
            txcharisk_r           <= fifo_dop( 3 downto  2);
         when send_word_hi =>
            txdata                <= txdata_r;
            txcharisk             <= txcharisk_r;
         when send_xon =>
            txdata                <= X"00" & K_XON;
            txcharisk             <= "01";
         when send_xoff =>
            txdata                <= X"00" & K_XOFF;
            txcharisk             <= "01";
         when others =>
            txdata                <= X"00" & k_idle;
            txcharisk             <= "01";
         end case;
      end if;
   end process;

   fifo_rden                      <= '1' when ((state = send_word_lo) and (fifo_empty = '0')) else '0';

   -----------------------------------------------------------------------------
   -- Tx flow control
   -----------------------------------------------------------------------------
   comma_lock_axi_gtx_sync_block_inst: entity axis_gtx_bridge_v1_0_lib.gtx_sync_block
   port map
   (
      clk                         =>  txusrclk,
      data_in                     =>  comma_lock_axi,
      data_out                    =>  comma_lock_axi_sync
   );

   k_idle                         <= K_IDLE_AXI when (comma_lock_axi_sync = '1') else K_IDLE_PLB;

   rx_xoff_gtx_sync_block_inst: entity axis_gtx_bridge_v1_0_lib.gtx_sync_block
   port map
   (
      clk                         =>  txusrclk,
      data_in                     =>  rx_xoff,
      data_out                    =>  rx_xoff_sync
   );

   tx_xoff_gtx_sync_block_inst: entity axis_gtx_bridge_v1_0_lib.gtx_sync_block
   port map
   (
      clk                         =>  txusrclk,
      data_in                     =>  tx_xoff,
      data_out                    =>  tx_xoff_sync
   );

   process(txusrclk)
   begin
      if (txusrclk'event and (txusrclk = '1')) then
         case (state) is
         when send_xon | send_xoff =>
            tx_xoff_timer         <= TX_XOFF_CYCLES;

         when others =>
            if (tx_xoff_timer /= 0) then
               tx_xoff_timer      <= tx_xoff_timer - 1;
            end if;
         end case;
      end if;
   end process;

end architecture structural;

--------------------------------------------------------------------------------
-- End of file
--------------------------------------------------------------------------------
