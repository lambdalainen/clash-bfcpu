module SSeg.SSeg (ssegH, ssegU) where

import CLaSH.Prelude
import SSeg.HexToSSeg

type Hex     = Unsigned 4
type Counter = Unsigned 20

hexMux :: Counter ->
          (Hex, Hex, Hex, Hex, Hex, Hex, Hex, Hex) ->
          (Unsigned 8, Hex)
hexMux counter (hex7, hex6, hex5, hex4, hex3, hex2, hex1, hex0) =
  case counter `shiftR` 17 of -- TODO: a more direct way to access the bits?
    0b000 -> (0b11111110, hex0)
    0b001 -> (0b11111101, hex1)
    0b010 -> (0b11111011, hex2)
    0b011 -> (0b11110111, hex3)
    0b100 -> (0b11101111, hex4)
    0b101 -> (0b11011111, hex5)
    0b110 -> (0b10111111, hex6)
    _     -> (0b01111111, hex7)

-- dp is ignored
ssegH :: Signal (Hex, Hex, Hex, Hex, Hex, Hex, Hex, Hex) ->
         Signal (Unsigned 8, BitVector 8)
ssegH h = (\(an, hex) -> (an, hexToSSegCA 0 hex)) <$> an_hex
  where
  an_hex = hexMux <$> counter <*> h

  counter :: Signal Counter
  counter = register 0 (counter + 1)

-- from Data.BitVector
(@@) :: (Integral a, Integral b, Bits a) => a -> (Int, Int) -> b
a @@ (j,i) = fromIntegral $ (a `shiftR` i) `mod` 2^m
  where
  m  = j - i + 1 -- width

ssegU :: Signal (Unsigned 32) -> Signal (Unsigned 8, BitVector 8)
ssegU n = ssegH (split <$> n)
  where
  split x = let f = (x @@)
            in (f  (31, 28), f  (27, 24), f  (23, 20), f  (19, 16),
                f  (15, 12), f  (11, 8),  f  (7,  4),  f  (3,  0))

{-# ANN topEntity
  (defTop
    { t_name     = "sseg"
    , t_inputs   = []
    , t_outputs  = ["an", "seg"]
    , t_extraIn  = [("clk", 1), ("btnCpuReset", 1)]
    , t_extraOut = []
    , t_clocks   = []
    }) #-}
topEntity = ssegU
