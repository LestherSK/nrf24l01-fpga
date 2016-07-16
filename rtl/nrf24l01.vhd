--------------------------------------------------------------------------------
-- PROJECT: nRF24L01(+) CONTROLLER FOR FPGA
--------------------------------------------------------------------------------
-- MODULE NAME: NRF24L01
-- AUTHORS:     Jakub Cabal <jakubcabal@gmail.com>
-- LICENSE:     LGPL-3.0, please read LICENSE file
-- WEBSITE:     https://github.com/jakubcabal/nrf24l01_fpga
-- USED TOOLS:  Quartus II 13.0 SP1
-- CREATE DATE: 16.07.‎2016
--------------------------------------------------------------------------------
-- COPYRIGHT NOTICE:
--------------------------------------------------------------------------------
-- nRF24L01(+) CONTROLLER FOR FPGA
-- Copyright (C) 2016 Jakub Cabal
--
-- This source file is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This source file is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;

entity NRF24L01 is
    Generic (
        -- SPI MASTER CONFIGURATIONS
        CLK_FREQ  : integer := 50; -- set system clock frequency in MHz
        SCLK_FREQ : integer := 5;  -- set SPI clock frequency in MHz (must be < CLK_FREQ/9)
        -- NRF24L01(+) CONFIGURATIONS
        RF_PAYLOAD_WIDTH : integer := 32;
        RF_CRC_EN        : std_logic := '1'; -- '0' = disable, '1' = enable
        RF_CRC_LENGHT    : std_logic := '0'; -- '0' = 8b, '1' = 16b
        RF_RX_PIPE0_ADDR : std_logic_vector(39 downto 0) := X"E8E8F0F0E1";
        RF_CHANNEL       : std_logic_vector(6 downto 0) := "1101100";
        -- RF_DATA_RATE: "00" = 1Mbps, "01" = 2Mbps, "10" = 256kbps
        RF_DATA_RATE     : std_logic_vector(1 downto 0) := "10";
        -- RF_POWER: "00" = -18dBm, "01" = -12dBm, "10" = -6dBm, "11" = 0dBm
        RF_POWER         : std_logic_vector(1 downto 0) := "11"
    );
    Port (
        CLK       : in  std_logic; -- system clock
        RST       : in  std_logic; -- high active synchronous reset
        -- SPI MASTER INTERFACE
        SCLK      : out std_logic;
        CS_N      : out std_logic;
        MOSI      : out std_logic;
        MISO      : in  std_logic;
        -- NRF24L01 INTERFACE
        RF_IRQ    : in  std_logic;
        RF_CE     : out std_logic;
        -- CONTROL INTERFACE
        STATUS_RD : in  std_logic; -- for debug
        -- USER DATA INTERFACE
        DOUT      : out std_logic_vector(255 downto 0); -- output data
        DOUT_VLD  : out std_logic -- when DOUT_VLD = 1, output data are valid
    );
end NRF24L01;

architecture FULL of NRF24L01 is

    signal wait_cnt          : unsigned(16 downto 0);
    signal wait_cnt_en       : std_logic;
    signal wait_130_cnt_max  : std_logic;
    signal wait_1500_cnt_max : std_logic;

    signal cmod_cmd          : std_logic_vector(7 downto 0);
    signal cmod_din          : std_logic_vector(255 downto 0);
    signal cmod_din_en       : std_logic;
    signal cmod_din_lng      : std_logic_vector(5 downto 0);
    signal cmod_valid        : std_logic;
    signal cmod_ready        : std_logic;
    signal cmod_status       : std_logic_vector(7 downto 0);
    signal cmod_status_vld   : std_logic;
    signal cmod_dout         : std_logic_vector(255 downto 0);
    signal cmod_dout_vld     : std_logic;

    signal rf_ce_sig         : std_logic;
    signal dout_reg          : std_logic_vector(255 downto 0);
    signal status_reg        : std_logic_vector(7 downto 0);
    signal dout_vld_reg      : std_logic;

    type state is (idle, set_config, wait_1500, set_channel, set_setup,
                   set_rx_addr_p0, set_rx_pw_p0, wait_130, rx_mode, read_data, wait_data_vld);
    signal present_state, next_state : state;

begin

    RF_CE    <= rf_ce_sig;
    DOUT     <= dout_reg;
    DOUT_VLD <= dout_vld_reg;

    -- -------------------------------------------------------------------------
    -- WAITING COUNTER
    -- -------------------------------------------------------------------------

    wait_cnt_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (wait_cnt_en = '1') then
                wait_cnt <= wait_cnt + 1;
            else
                wait_cnt <= (others => '0');
            end if;
        end if;
    end process;

    -- -------------------------------------------------------------------------
    -- WAITING FLAG REGISTERS
    -- -------------------------------------------------------------------------

    wait_130_cnt_max_reg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (wait_cnt = "00001101101011000") then
                wait_130_cnt_max <= '1';
            else
                wait_130_cnt_max <= '0';
            end if;
        end if;
    end process;

    wait_1500_cnt_max_reg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (wait_cnt = "10011100010000000") then
                wait_1500_cnt_max <= '1';
            else
                wait_1500_cnt_max <= '0';
            end if;
        end if;
    end process;

    -- -------------------------------------------------------------------------
    -- STATUS REGISTER
    -- -------------------------------------------------------------------------

    status_reg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (RST = '1') then
                status_reg <= (others => '0');
            elsif (cmod_status_vld = '1') then
                status_reg <= cmod_status;
            end if;
        end if;
    end process;

    -- -------------------------------------------------------------------------
    -- OUTPUT DATA REGISTERS
    -- -------------------------------------------------------------------------

    dout_reg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (STATUS_RD = '1') then
                dout_reg <= (others => '0');
                dout_reg(7 downto 0) <= status_reg;
            else
                dout_reg <= cmod_dout;
            end if;
        end if;
    end process;

    dout_vld_reg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (RST = '1') then
                dout_vld_reg <= '0';
            else
                dout_vld_reg <= (cmod_dout_vld AND rf_ce_sig) OR STATUS_RD;
            end if;
        end if;
    end process;

    -- -------------------------------------------------------------------------
    -- NRF24L01 FSM
    -- -------------------------------------------------------------------------

    -- PRESENT STATE REGISTER
    process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (RST = '1') then
                present_state <= idle;
            else
                present_state <= next_state;
            end if;
        end if;
    end process;

    -- NEXT STATE AND OUTPUTS LOGIC
    process (present_state, cmod_ready, wait_1500_cnt_max, wait_130_cnt_max, RF_IRQ, cmod_dout_vld)
    begin

        case present_state is

            when idle =>
                cmod_cmd     <= (others => '0');
                cmod_din     <= (others => '0');
                cmod_din_en  <= '0';
                cmod_din_lng <= (others => '0');
                cmod_valid   <= '0';
                wait_cnt_en  <= '0';
                rf_ce_sig    <= '0';

                if (cmod_ready = '1') then
                    next_state <= set_config;
                else
                    next_state <= idle;
                end if;

            when set_config =>
                cmod_cmd     <= "00100000"; -- write to control register
                cmod_din     <= (255 downto 4 => '0') & RF_CRC_EN & RF_CRC_LENGHT & "11";
                cmod_din_en  <= '1';
                cmod_din_lng <= "000001";
                cmod_valid   <= '1';
                wait_cnt_en  <= '0';
                rf_ce_sig    <= '0';

                if (cmod_ready = '1') then
                    next_state <= wait_1500;
                else
                    next_state <= set_config;
                end if;

            when wait_1500 =>
                cmod_cmd     <= (others => '0');
                cmod_din     <= (others => '0');
                cmod_din_en  <= '0';
                cmod_din_lng <= (others => '0');
                cmod_valid   <= '0';
                wait_cnt_en  <= '1';
                rf_ce_sig    <= '0';

                if (wait_1500_cnt_max = '1') then
                    next_state <= set_channel;
                else
                    next_state <= wait_1500;
                end if;

            when set_channel =>
                cmod_cmd     <= "00100101"; -- write to channel register
                cmod_din     <= (255 downto 7 => '0') & RF_CHANNEL; -- write channel
                cmod_din_en  <= '1';
                cmod_din_lng <= "000001";
                cmod_valid   <= '1';
                wait_cnt_en  <= '0';
                rf_ce_sig    <= '0';

                if (cmod_ready = '1') then
                    next_state <= set_setup;
                else
                    next_state <= set_channel;
                end if;

            when set_setup =>
                cmod_cmd     <= "00100110"; -- write to setup register
                cmod_din     <= (255 downto 6 => '0') & RF_DATA_RATE(1) & '0' & RF_DATA_RATE(0) & RF_POWER & '0';
                cmod_din_en  <= '1';
                cmod_din_lng <= "000001";
                cmod_valid   <= '1';
                wait_cnt_en  <= '0';
                rf_ce_sig    <= '0';

                if (cmod_ready = '1') then
                    next_state <= set_rx_addr_p0;
                else
                    next_state <= set_setup;
                end if;

            when set_rx_addr_p0 =>
                cmod_cmd     <= "00101010"; -- write to rx addr pipe0 register
                cmod_din     <= (255 downto 40 => '0') & RF_RX_PIPE0_ADDR;
                cmod_din_en  <= '1';
                cmod_din_lng <= "000101";
                cmod_valid   <= '1';
                wait_cnt_en  <= '0';
                rf_ce_sig    <= '0';

                if (cmod_ready = '1') then
                    next_state <= set_rx_pw_p0;
                else
                    next_state <= set_rx_addr_p0;
                end if;

            when set_rx_pw_p0 =>
                cmod_cmd     <= "00110001"; -- write to rx payload width pipe0 register
                cmod_din     <= (255 downto 6 => '0') & std_logic_vector(to_unsigned(RF_PAYLOAD_WIDTH,6));
                cmod_din_en  <= '1';
                cmod_din_lng <= "000001";
                cmod_valid   <= '1';
                wait_cnt_en  <= '0';
                rf_ce_sig    <= '0';

                if (cmod_ready = '1') then
                    next_state <= wait_130;
                else
                    next_state <= set_rx_pw_p0;
                end if;

            when wait_130 =>
                cmod_cmd     <= (others => '0');
                cmod_din     <= (others => '0');
                cmod_din_en  <= '0';
                cmod_din_lng <= (others => '0');
                cmod_valid   <= '0';
                wait_cnt_en  <= '1';
                rf_ce_sig    <= '0';

                if (wait_130_cnt_max = '1') then
                    next_state <= rx_mode;
                else
                    next_state <= wait_130;
                end if;

            when rx_mode =>
                cmod_cmd     <= (others => '0');
                cmod_din     <= (others => '0');
                cmod_din_en  <= '0';
                cmod_din_lng <= (others => '0');
                cmod_valid   <= '0';
                wait_cnt_en  <= '0';
                rf_ce_sig    <= '1';

                if (RF_IRQ = '0') then
                    next_state <= read_data;
                else
                    next_state <= rx_mode;
                end if;

            when read_data =>
                cmod_cmd     <= "01100001"; -- read rx data
                cmod_din     <= (others => '0');
                cmod_din_en  <= '1';
                cmod_din_lng <= "100000";
                cmod_valid   <= '1';
                wait_cnt_en  <= '0';
                rf_ce_sig    <= '1';

                if (cmod_ready = '1') then
                    next_state <= wait_data_vld;
                else
                    next_state <= read_data;
                end if;

            when wait_data_vld =>
                cmod_cmd     <= (others => '0');
                cmod_din     <= (others => '0');
                cmod_din_en  <= '0';
                cmod_din_lng <= (others => '0');
                cmod_valid   <= '0';
                wait_cnt_en  <= '0';
                rf_ce_sig    <= '1';

                if (cmod_dout_vld = '1') then
                    next_state <= rx_mode;
                else
                    next_state <= wait_data_vld;
                end if;

            when others =>
                cmod_cmd     <= (others => '0');
                cmod_din     <= (others => '0');
                cmod_din_en  <= '0';
                cmod_din_lng <= (others => '0');
                cmod_valid   <= '0';
                wait_cnt_en  <= '0';
                rf_ce_sig    <= '0';

        end case;
    end process;

    -- -------------------------------------------------------------------------
    -- NRF24L01 COMMAND MODULE
    -- -------------------------------------------------------------------------

    nrf24l01_cmd_i : entity work.NRF24L01_CMD
    generic map(
        CLK_FREQ   => CLK_FREQ, -- set system clock frequency in MHz
        SCLK_FREQ  => SCLK_FREQ -- set SPI clock frequency in MHz (must be < CLK_FREQ/9)
    )
    port map(
        CLK        => CLK,
        RST        => RST,
        -- SPI MASTER INTERFACE
        SCLK       => SCLK,
        CS_N       => CS_N,
        MOSI       => MOSI,
        MISO       => MISO,
        -- USER INTERFACE
        CMD        => cmod_cmd,
        DIN        => cmod_din,
        DIN_EN     => cmod_din_en,
        DIN_LNG    => cmod_din_lng,
        VALID      => cmod_valid,
        READY      => cmod_ready,
        STATUS     => cmod_status,
        STATUS_VLD => cmod_status_vld,
        DOUT       => cmod_dout,
        DOUT_VLD   => cmod_dout_vld
    );

end FULL;
