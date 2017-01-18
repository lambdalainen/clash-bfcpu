module UART.UART (uartRx, uartTx, topEntity) where

import CLaSH.Prelude
import Data.Char
import UART.Baud
import UART.Rx
import UART.Tx

uartRx :: Signal Bit -> (Signal (Unsigned 8), Signal Bool)
uartRx rx = (rx_dout, rx_done_tick)
  where
  (rx_dout, rx_done_tick) = mealyB rxRun rxInit (rx, baud_tick)

uartTx :: Signal Bool -> Signal (Unsigned 8) -> (Signal Bit, Signal Bool)
uartTx tx_start tx_din = (tx, tx_done_tick)
  where
  (tx, tx_done_tick) = mealyB txRun txInit (tx_start, tx_din, baud_tick)

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
topEntity rx = tx
  where
  (rx_dout, rx_done_tick) = uartRx rx
  (tx, tx_done_tick)      = uartTx rx_done_tick rx_dout
