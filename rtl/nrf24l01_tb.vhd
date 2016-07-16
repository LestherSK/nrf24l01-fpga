--------------------------------------------------------------------------------
-- PROJECT: nRF24L01(+) CONTROLLER FOR FPGA
--------------------------------------------------------------------------------
-- MODULE NAME: NRF24L01_TB
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

entity NRF24L01_TB is
end NRF24L01_TB;

architecture FULL of NRF24L01_TB is

    constant CLK_PERIOD  : time := 20 ns;
    constant DATA_VALUE  : std_logic_vector(7 downto 0) := X"12";
    constant DATA_VALUE2 : std_logic_vector(7 downto 0) := X"F4";
    constant DATA_VALUE3 : std_logic_vector(7 downto 0) := X"47";

    signal CLK       : std_logic := '0';
    signal RST       : std_logic := '1';

    signal sclk      : std_logic;
    signal cs_n      : std_logic;
    signal miso      : std_logic;
    signal mosi      : std_logic;

    signal rf_irq    : std_logic;
    signal rf_ce     : std_logic;
    signal status_rd : std_logic;

    signal dout      : std_logic_vector(255 downto 0);
    signal dout_vld  : std_logic;

begin

    utt : entity work.NRF24L01
    port map(
        CLK       => CLK,
        RST       => RST,
        -- SPI MASTER INTERFACE
        SCLK      => sclk,
        CS_N      => cs_n,
        MOSI      => mosi,
        MISO      => miso,
        -- NRF24L01 INTERFACE
        RF_IRQ    => rf_irq,
        RF_CE     => rf_ce,
        -- CONTROL INTERFACE
        STATUS_RD => status_rd,
        -- USER DATA INTERFACE
        DOUT      => dout,
        DOUT_VLD  => dout_vld
    );

    clk_process : process
    begin
        CLK <= '1';
        wait for CLK_PERIOD/2;
        CLK <= '0';
        wait for CLK_PERIOD/2;
    end process;

    rst_process : process
    begin
        RST <= '1';
        wait for 40 ns;
        RST <= '0';
        wait;
    end process;

    rf_irq_process : process
    begin
        rf_irq <= '1';
        status_rd <= '0';
        wait for 1800 us;
        rf_irq <= '0';
        wait for 30 us;
        rf_irq <= '1';
        wait;
    end process;

    test_process : process
    begin
        miso <= '0';

        wait until RST = '0';

        test_loop : for j in 0 to 46 loop
            for i in (DATA_VALUE'LENGTH-1) downto 0 loop
                miso <= DATA_VALUE(i);
                wait until falling_edge(sclk);
            end loop;

            for i in (DATA_VALUE2'LENGTH-1) downto 0 loop
    			miso <= DATA_VALUE2(i);
                wait until falling_edge(sclk);
    		end loop;

            for i in (DATA_VALUE3'LENGTH-1) downto 0 loop
                miso <= DATA_VALUE3(i);
                wait until falling_edge(sclk);
            end loop;
        end loop;

        wait;
    end process;

end FULL;
