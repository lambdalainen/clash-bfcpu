{-# LANGUAGE RecordWildCards #-}

data MachCode = MachCode
  { fstInt    :: Int
  , sndString :: String
  }

nullCode = MachCode { fstInt = 0, sndString = "hello" }

cpu :: Int -> (Int, String)
cpu n = (fstInt, sndString)
  where
  MachCode {..} = case n of
    0 -> nullCode { fstInt = 1 }
    _ -> nullCode { sndString = "world" }

cpu2 :: Int -> (Int, String)
cpu2 n = pair
  where
  pair = (fstInt, sndString)
  MachCode {..} = case n of
    0 -> nullCode { fstInt = 1 }
    _ -> nullCode { sndString = "world" }

cpu3 :: MachCode -> (Int, String)
cpu3 MachCode { fstInt = i } = (fstInt, sndString)
  where
  MachCode {..} = case i of
    0 -> nullCode { fstInt = 1 }
    _ -> nullCode { sndString = "world" }
