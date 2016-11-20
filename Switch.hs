module Switch where

import CLaSH.Prelude

-- This is the expected behavior: when switch is on, the led
-- will have half of its brightness, when switch is off, the led
-- is randomly off or on.
--
-- *Switch> sampleN 10 (topEntity $ pure False)
-- [False,False,False,False,False,False,False,False,False,False]
-- *Switch> sampleN 10 (topEntity $ pure True)
-- [False,True,False,True,False,True,False,True,False,True]


{-# ANN topEntity
  (defTop
    { t_name     = "up_counter"
    , t_inputs   = ["sw"]
    , t_outputs  = ["led"]
    , t_extraIn  = [("clk", 1)]
    , t_extraOut = []
    , t_clocks   = []
    }) #-}
topEntity :: Signal Bool -> Signal Bool
topEntity sw = s
  where
  s = register False (toggle <$> sw <*> s)

  toggle b s = if b then not s else s
