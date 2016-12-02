module SSeg.HexToSSeg where

import CLaSH.Prelude

hexToSSeg :: Unsigned 4 -> Bit -> BitVector 8
hexToSSeg hex dp =
  case hex of
    0x0 -> dp ++# 0b0000001
    0x1 -> dp ++# 0b1001111
    0x2 -> dp ++# 0b0010010
    0x3 -> dp ++# 0b0000110
    0x4 -> dp ++# 0b1001100
    0x5 -> dp ++# 0b0100100
    0x6 -> dp ++# 0b0100000
    0x7 -> dp ++# 0b0001111
    0x8 -> dp ++# 0b0000000
    0x9 -> dp ++# 0b0000100
    0xA -> dp ++# 0b0001000
    0xB -> dp ++# 0b1100000
    0xC -> dp ++# 0b0110001
    0xD -> dp ++# 0b1000010
    0xE -> dp ++# 0b0110000
    _   -> dp ++# 0b0111000
