module UART.UART (uart) where

import CLaSH.Prelude
import UART.Baud
import UART.Rx

-- {-# ANN topEntity
--   (defTop
--     { t_name     = "uart"
--     , t_inputs   = ["RsRx"]
--     , t_outputs  = ["led"]
--     , t_extraIn  = [("clk", 1)]
--     , t_extraOut = []
--     , t_clocks   = []
--     }) #-}
-- topEntity :: Signal Bit -> Signal (BitVector 8)
-- topEntity rx = _b_reg <$> rx_reg
--   where
--   rx_reg  = register rxInit (rxRun <$> rx_reg <*> rx <*> baud_tick)

-- rx -> (rx_done_tick, dout)
uart :: Signal Bit -> (Signal Bit, Signal (BitVector 8))
uart rx = (_rx_done_tick <$> rx_reg, _b_reg <$> rx_reg)
  where
  rx_reg  = register rxInit (rxRun <$> rx_reg <*> rx <*> baud_tick)
