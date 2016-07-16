--------------------------------------------------------------------------------
-- PROJECT: nRF24L01(+) CONTROLLER FOR FPGA
--------------------------------------------------------------------------------
-- MODULE NAME: NRF24L01_CMD
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

entity NRF24L01_CMD is
    Generic (
        -- SPI MASTER CONFIGURATIONS
        CLK_FREQ   : integer := 50; -- set system clock frequency in MHz
        SCLK_FREQ  : integer := 5   -- set SPI clock frequency in MHz (must be < CLK_FREQ/9)
    );
    Port (
        CLK        : in  std_logic; -- system clock
        RST        : in  std_logic; -- high active synchronous reset
        -- SPI MASTER INTERFACE
        SCLK       : out std_logic;
        CS_N       : out std_logic;
        MOSI       : out std_logic;
        MISO       : in  std_logic;
        -- USER INTERFACE
        CMD        : in  std_logic_vector(7 downto 0); -- input command
        DIN        : in  std_logic_vector(255 downto 0); -- input data
        DIN_EN     : in  std_logic; -- when DIN_EN = 1, input data will be used
        DIN_LNG    : in  std_logic_vector(4 downto 0); -- input data long in bytes (1B to 32B)
        VALID      : in  std_logic; -- when VALID = 1, DIN, DIN_EN, DIN_LNG and CMD are valid and can be accept
        READY      : out std_logic; -- when READY = 1, NRF24L01_CMD is ready to accept next command and data
        STATUS     : out std_logic_vector(7 downto 0); -- status data
        STATUS_VLD : out std_logic; -- when STATUS_VLD = 1, status data are valid
        DOUT       : out std_logic_vector(255 downto 0); -- output data
        DOUT_VLD   : out std_logic  -- when DOUT_VLD = 1, output data are valid
    );
end NRF24L01_CMD;

architecture FULL of NRF24L01_CMD is

    signal spi_din          : std_logic_vector(7 downto 0);
    signal spi_din_vld      : std_logic;
    signal spi_ready        : std_logic;
    signal spi_dout         : std_logic_vector(7 downto 0);
    signal spi_dout_vld     : std_logic;

    signal cmd_reg          : std_logic_vector(7 downto 0);
    signal data_reg         : std_logic_vector(255 downto 0);
    signal data_en_reg      : std_logic;
    signal data_lng_reg     : std_logic_vector(4 downto 0);

    signal byte_cnt         : unsigned(4 downto 0);
    signal byte_num         : integer;
    signal byte_cnt_max     : std_logic;
    signal byte_cnt_en      : std_logic;
    signal byte_cnt_tick    : std_logic;

    signal dout_reg         : std_logic_vector(255 downto 0);
    signal dout_reg_en      : std_logic;
    signal dout_comb        : std_logic_vector(255 downto 0);
    signal dout_vld_sig     : std_logic;

    signal ready_sig        : std_logic;
    signal status_vld_sig   : std_logic;

    type state is (idle, command, wait_status_vld, data, wait_data_vld);
    signal present_state, next_state : state;

begin

    READY         <= ready_sig;
    STATUS        <= spi_dout;
    STATUS_VLD    <= status_vld_sig;
    DOUT          <= dout_reg;
    byte_cnt_tick <= spi_ready;
    byte_num      <= to_integer(byte_cnt);
    dout_reg_en   <= dout_vld_sig;

    -- -------------------------------------------------------------------------
    -- INPUT REGISTERS
    -- -------------------------------------------------------------------------

    input_regs_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (RST = '1') then
                cmd_reg      <= (others => '0');
                data_reg     <= (others => '0');
                data_en_reg  <= '0';
                data_lng_reg <= (others => '0');
            elsif (VALID = '1' AND ready_sig = '1') then
                cmd_reg      <= CMD;
                data_reg     <= DIN;
                data_en_reg  <= DIN_EN;
                data_lng_reg <= DIN_LNG;
            end if;
        end if;
    end process;

    -- -------------------------------------------------------------------------
    -- DATA BYTE COUNTER AND BYTE CNT MAX FLAG REGISTER
    -- -------------------------------------------------------------------------

    byte_cnt_reg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (RST = '1') then
                byte_cnt <= (others => '0');
            elsif (byte_cnt_en = '1' AND byte_cnt_tick = '1') then
                if (byte_cnt = unsigned(data_lng_reg)) then
                    byte_cnt <= (others => '0');
                else
                    byte_cnt <= byte_cnt + 1;
                end if;
            end if;
        end if;
    end process;

    byte_cnt_max_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (byte_cnt = unsigned(data_lng_reg)) then
                byte_cnt_max <= '1';
            else
                byte_cnt_max <= '0';
            end if;
        end if;
    end process;

    -- -------------------------------------------------------------------------
    -- OUTPUT DATA LOGIC AND REGISTER
    -- -------------------------------------------------------------------------

    dout_comb_p : process (byte_cnt_en, dout_reg, byte_num, spi_dout)
    begin
        if (byte_cnt_en = '1') then
            dout_comb <= dout_reg;
            dout_comb((8*byte_num)+15 downto (8*byte_num)+8) <= spi_dout;
        else
            dout_comb <= dout_reg;
            dout_comb(255 downto 248) <= spi_dout;
        end if;
    end process;

    dout_reg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (dout_reg_en = '1') then
                dout_reg <= dout_comb;
            end if;
        end if;
    end process;

    dout_vld_reg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (RST = '1') then
                DOUT_VLD <= '0';
            else
                DOUT_VLD <= dout_vld_sig;
            end if;
        end if;
    end process;

    -- -------------------------------------------------------------------------
    -- NRF24L01 COMMAND FSM
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
    process (present_state, VALID, cmd_reg, data_en_reg, spi_dout_vld,
             spi_ready, data_reg, byte_cnt_max, byte_num)
    begin

        case present_state is

            when idle =>
                spi_din        <= (others => '0');
                spi_din_vld    <= '0';
                ready_sig      <= '1';
                status_vld_sig <= '0';
                byte_cnt_en    <= '0';
                dout_vld_sig   <= '0';

                if (VALID = '1') then
                    next_state <= command;
                else
                    next_state <= idle;
                end if;

            when command =>
                spi_din        <= cmd_reg;
                spi_din_vld    <= '1';
                ready_sig      <= '0';
                status_vld_sig <= '0';
                byte_cnt_en    <= '0';
                dout_vld_sig   <= '0';

                if (spi_ready = '1') then
                    next_state <= wait_status_vld;
                else
                    next_state <= command;
                end if;

            when wait_status_vld =>
                spi_din        <= (others => '0');
                spi_din_vld    <= '0';
                ready_sig      <= '0';
                status_vld_sig <= spi_dout_vld;
                byte_cnt_en    <= '0';
                dout_vld_sig   <= '0';

                if (spi_dout_vld = '1' AND data_en_reg = '1') then
                    next_state <= data;
                elsif (spi_dout_vld = '1' AND data_en_reg = '0') then
                    next_state <= idle;
                else
                    next_state <= wait_status_vld;
                end if;

            when data =>
                spi_din        <= data_reg((8*byte_num)+7 downto (8*byte_num));
                spi_din_vld    <= '1';
                ready_sig      <= '0';
                status_vld_sig <= '0';
                byte_cnt_en    <= '1';
                dout_vld_sig   <= '0';

                if (spi_ready = '1' AND byte_cnt_max = '1') then
                    next_state <= wait_data_vld;
                else
                    next_state <= data;
                end if;

            when wait_data_vld =>
                spi_din        <= (others => '0');
                spi_din_vld    <= '0';
                ready_sig      <= '0';
                status_vld_sig <= '0';
                byte_cnt_en    <= '0';
                dout_vld_sig   <= spi_dout_vld;

                if (spi_dout_vld = '1') then
                    next_state <= idle;
                else
                    next_state <= wait_data_vld;
                end if;

            when others =>
                spi_din        <= (others => '0');
                spi_din_vld    <= '0';
                ready_sig      <= '0';
                status_vld_sig <= '0';
                byte_cnt_en    <= '0';
                dout_vld_sig   <= '0';
                next_state     <= idle;

        end case;
    end process;

    -- -------------------------------------------------------------------------
    -- SPI MASTER CONTROLLER
    -- -------------------------------------------------------------------------

    spi_master_i : entity work.SPI_MASTER
    generic map(
        CLK_FREQ   => CLK_FREQ,  -- set system clock frequency in MHz
        SCLK_FREQ  => SCLK_FREQ, -- set SPI clock frequency in MHz (must be < CLK_FREQ/9)
        DATA_WIDTH => 8          -- set SPI datawidth in bits
    )
    port map(
        CLK      => CLK,
        RST      => RST,
        -- SPI MASTER INTERFACE
        SCLK     => SCLK,
        CS_N     => CS_N,
        MOSI     => MOSI,
        MISO     => MISO,
        -- USER INTERFACE
        DIN      => spi_din,
        DIN_VLD  => spi_din_vld,
        READY    => spi_ready,
        DOUT     => spi_dout,
        DOUT_VLD => spi_dout_vld
    );

end FULL;
