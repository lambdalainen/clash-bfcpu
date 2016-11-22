module UART.UART (uartRx, uartTx) where

import CLaSH.Prelude
import Data.Char
import UART.Baud
import UART.Rx
import UART.Tx

-- {-# ANN topEntity
--   (defTop
--     { t_name     = "uart"
--     , t_inputs   = ["RsRx"]
--     , t_outputs  = ["RsTx"]
--     , t_extraIn  = [("clk", 1)]
--     , t_extraOut = []
--     , t_clocks   = []
--     }) #-}
-- topEntity :: Signal Bit -> Signal Bit
-- topEntity rx = _tx <$> tx_reg
--   where
--   rx_reg  = register rxInit (rxRun <$> rx_reg <*> rx <*> baud_tick)
--   tx_reg  = register txInit (txRun <$> tx_reg <*> (_rx_done_tick <$> rx_reg)
--                                               <*> (_tx_dout <$> rx_reg)
--                                               <*> baud_tick)

uartRx :: Signal Bit -> (Signal (Unsigned 8), Signal Bool)
uartRx rx = (_rx_dout <$> rx_reg, _rx_done_tick <$> rx_reg)
  where
  rx_reg  = register rxInit (rxRun <$> rx_reg <*> rx <*> baud_tick)

uartTx :: Signal Bool -> Signal (Unsigned 8) -> (Signal Bit, Signal Bool)
uartTx tx_start tx_din = (_tx <$> tx_reg, _tx_done_tick <$> tx_reg)
  where
  tx_reg  = register txInit (txRun <$> tx_reg <*> tx_start <*> tx_din <*> baud_tick)
