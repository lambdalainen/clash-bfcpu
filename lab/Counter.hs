module UpCounter where

import CLaSH.Prelude

-- regEn :: a -> Signal Bool -> Signal a -> Signal a Source
-- 
-- Version of register that only updates its content when its second
-- argument is asserted. So given:
-- 
-- oscillate = register False (not1 oscillate)
-- count     = regEn 0 oscillate (count + 1)
-- We get:
-- 
-- >>> sampleN 8 oscillate
-- [False,True,False,True,False,True,False,True]
-- >>> sampleN 8 count
-- [0,0,1,1,2,2,3,3]

-- Prelude> 2^28 / (100 * 10^6)
-- 2.68435456
upCounter :: Signal Bool -> Signal (Unsigned 28)
upCounter enable = s
  where
  s = regEn 0 enable (s + 1)

-- NOTE!
-- we could write (if counter == 268435455 then not1 s else s), however
-- this doesn't seem to work, at least there will be error when:
--
-- *UpCounter> sampleN 10 (topEntity $ pure True)
-- [False*** Exception: (==)' undefined for 'Signal'', use '(.==.)' instead
ledState :: Signal (Unsigned 28) -> Signal Bool
ledState counter = s
  where
  s = register False (toggle <$> counter <*> s)
  
  toggle c s = if c == 268435455 then not s else s
  

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
topEntity = ledState . upCounter
