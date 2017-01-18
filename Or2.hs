module Or2 where

import CLaSH.Prelude hiding (or)

--or :: Signal Bit -> Signal Bit -> Signal Bit
or a b = a .|. b

or_comp :: Unsigned 7 -> Unsigned 7 -> Unsigned 7
or_comp a b = complement (a `or` b)
