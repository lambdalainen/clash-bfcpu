module Or where

import CLaSH.Prelude

{-# ANN topEntity
  (defTop
    { t_name     = "or_gate"
    , t_inputs   = ["sw0", "sw1"]
    , t_outputs  = ["led"]
    , t_extraIn  = []
    , t_extraOut = []
    , t_clocks   = []
    }) #-}
topEntity :: BitVector 2 -> BitVector 2 -> Bool
topEntity in_0 in_1 =
  case (in_0, in_1) of
    (0x00, 0x00) -> False
    _            -> True
