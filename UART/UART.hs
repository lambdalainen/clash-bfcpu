module UART.UART (uartRx, uartTx, topEntity) where

import CLaSH.Prelude
import Data.Char
import Types
import UART.Baud
import UART.FIFO
import UART.Rx
import UART.Tx

uartRx :: Signal Bit -> Signal Bool -> (Signal Data, Signal Bool)
uartRx rx rx_read = (fifo_r_data, fifo_empty)
  where
  (rx_dout, rx_done_tick) = mealyB rxRun rxInit (rx, baud_tick)

  (fifo_empty, _, fifo_r_data) = fifo rx_read rx_done_tick rx_dout

uartTx :: Signal Bool -> Signal Data -> (Signal Bit, Signal Bool)
uartTx tx_write tx_data = (tx, tx_done_tick)
  where
  (tx, tx_done_tick) = mealyB txRun txInit (tx_write, tx_data, baud_tick)

{-# ANN topEntity
  (defTop
    { t_name     = "uart"
    , t_inputs   = ["RsRx", "btnC"]
    , t_outputs  = ["RsTx"]
    , t_extraIn  = [("clk", 1)]
    , t_extraOut = []
    , t_clocks   = []
    }) #-}
topEntity :: Signal Bit -> Signal Bool -> Signal Bit
topEntity rx rx_read = tx
  where
  (rx_data, rx_empty) = uartRx rx rx_read
  (tx, tx_done_tick)  = uartTx rx_read rx_data
