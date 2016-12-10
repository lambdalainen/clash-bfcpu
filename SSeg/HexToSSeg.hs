module SSeg.HexToSSeg where

import CLaSH.Prelude

-- common anode
hexToSSegCA :: Bit -> Unsigned 4 -> BitVector 8
hexToSSegCA dp hex =
  case hex of
    0x0 -> dp' ++# 0b1000000
    0x1 -> dp' ++# 0b1111001
    0x2 -> dp' ++# 0b0100100
    0x3 -> dp' ++# 0b0110000
    0x4 -> dp' ++# 0b0011001
    0x5 -> dp' ++# 0b0010010
    0x6 -> dp' ++# 0b0000010
    0x7 -> dp' ++# 0b1111000
    0x8 -> dp' ++# 0b0000000
    0x9 -> dp' ++# 0b0010000
    0xA -> dp' ++# 0b0001000
    0xB -> dp' ++# 0b0000011
    0xC -> dp' ++# 0b1000110
    0xD -> dp' ++# 0b0100001
    0xE -> dp' ++# 0b0000110
    _   -> dp' ++# 0b0001110
  where
  dp' = complement dp

-- common cathode
hexToSSegCC :: Bit -> Unsigned 4 -> BitVector 8
hexToSSegCC dp hex = complement $ hexToSSegCA dp hex
