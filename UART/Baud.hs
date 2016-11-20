module UART.Baud (baud_tick) where

import CLaSH.Prelude

m :: BitVector 9
m = 326

baud_tick :: Signal Bit
baud_tick = tick <$> s
  where
  tick n = if n == m then 1 else 0
  inc n = if n == m then 0 else n + 1
  s = register 0 (inc <$> s)
