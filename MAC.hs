module MAC where

import CLaSH.Prelude

type U3  = Unsigned 3
type U32 = Unsigned 32

-- Type annotation will affect the generated Verilog. I think explictly set the width
-- here might be a good idea.
ma :: (U3, U32) -> (U3, U3) -> (U3, U32)
ma (acc, counter) (x,y) =
  if counter `mod` 100000000 == 0
    then (acc + x * y, counter + 1)
    else (acc, counter + 1)

-- macT state input = (newstate, output)
macT :: (U3, U32) -> (U3, U3) -> ((U3, U32), U3)
macT s@(acc, _) i = (ma s i, acc)

-- mealy :: (s -> i -> (s, o))       -- transfer function, see above
--       -> s                        -- initial state
--       -> (Signal i -> Signal o)   -- synchronous sequential function with input and output
--                                   -- matching that of the mealy machine

-- moore :: (s -> i -> s)            -- state -> input -> newstate
--       -> (s -> o)                 -- output only depends on current state
--       -> s                        -- initial state
--       -> (Signal i -> Signal o)

{-# ANN topEntity
  (defTop
    { t_name     = "mac"
    , t_inputs   = ["sw0", "sw1"]
    , t_outputs  = ["led"]
    , t_extraIn  = [("clk", 1)]
    , t_extraOut = []
    , t_clocks   = []
    }) #-}
topEntity :: Signal (U3, U3) -> Signal (U3)
topEntity = mealy macT (0, 0)
