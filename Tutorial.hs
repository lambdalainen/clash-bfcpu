-- Create a constant Signal from a combinational value
signal :: a -> Signal a

>>> sampleN 5 (signal 4 :: Signal Int)
[4,4,4,4,4]

-- generalisation
signal :: Applicative f => a -> f a

-- initial value -> signal to delay for 1 cycle
register :: a -> Signal a -> Signal a 

>>> sampleN 3 (register 8 (fromList [1,2,3,4]))
[8, 1, 2]
