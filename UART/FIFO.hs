{-# LANGUAGE RecordWildCards #-}

module UART.FIFO where

import CLaSH.Prelude hiding (empty)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Types

type FifoAddr = Unsigned 8 -- this has to match the size of fifoRam

data FifoState = FifoState
  { _r_ptr  :: FifoAddr
  , _w_ptr  :: FifoAddr
  , _w_en   :: Bool
  , _empty  :: Bool
  , _full   :: Bool
  }

makeLenses ''FifoState

fifoInit :: FifoState
fifoInit = FifoState 0 0 False True False

fifoVec :: Vec (2 ^ 8) Data
fifoVec = replicate snat 0

fifoRam :: Signal FifoAddr -> Signal FifoAddr -> Signal Bool -> Signal Data -> Signal Data
fifoRam  = readNew $ blockRamPow2 fifoVec

fifoRun :: FifoState -> Bool -> Bool -> Data -> FifoState
fifoRun s@(FifoState {..}) rd wr w_data = flip execState s $ do
  w_en .= (wr && not _full)
  case (rd, wr) of
    (True, False) -> -- read
      when (not _empty) $ do
        r_ptr += 1
        full .= False
        when (_w_ptr == _r_ptr + 1) $
          empty .= True
    (False, True) -> -- write
      when (not _full) $ do
        w_ptr += 1
        empty .= False
        when (_r_ptr == _w_ptr + 1) $
          full .= True
    (True, True) -> do
      r_ptr += 1
      w_ptr += 1
    _ -> return ()

fifo :: Signal Bool -> Signal Bool -> Signal Data -> (Signal Bool, Signal Bool, Signal Data)
fifo rd wr w_data = (sig _empty, sig _full, r_data)
  where
  sig = (<$> fifo_reg)

  fifo_reg = register fifoInit (fifoRun <$> fifo_reg <*> rd <*> wr <*> w_data)

  r_data = fifoRam (sig _w_ptr) (sig _r_ptr) (sig _w_en) w_data

{-# ANN topEntity
  (defTop
    { t_name     = "fifo"
    , t_inputs   = ["btnL", "btnR", "sw"]
    , t_outputs  = ["led_empty", "led_full", "led"]
    , t_extraIn  = [("clk", 1), ("btnCpuReset", 1)]
    , t_extraOut = []
    , t_clocks   = []
    }) #-}
topEntity :: Signal Bool -> Signal Bool -> Signal Data -> (Signal Bool, Signal Bool, Signal Data)
topEntity = fifo
