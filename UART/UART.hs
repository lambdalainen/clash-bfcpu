module UART.UART where

import CLaSH.Prelude
import Data.Char
import UART.Baud
import UART.Rx
import UART.Tx

{-# ANN topEntity
  (defTop
    { t_name     = "uart"
    , t_inputs   = ["RsRx"]
    , t_outputs  = ["RsTx"]
    , t_extraIn  = [("clk", 1)]
    , t_extraOut = []
    , t_clocks   = []
    }) #-}
topEntity :: Signal Bit -> Signal Bit
topEntity rx = _tx <$> tx_reg
  where
  rx_reg  = register rxInit (rxRun <$> rx_reg <*> rx <*> baud_tick)
  tx_reg  = register txInit (txRun <$> tx_reg <*> (_rx_done_tick <$> rx_reg)
                                              <*> baud_tick
                                              <*> (_b_reg <$> rx_reg))

-- -- rx -> (rx_done_tick, dout)
-- uart :: Signal Bit -> (Signal Bit, Signal (Unsigned 8))
-- uart rx = (_rx_done_tick <$> rx_reg, _b_reg <$> rx_reg)
--   where
--   rx_reg  = register rxInit (rxRun <$> rx_reg <*> rx <*> baud_tick)
