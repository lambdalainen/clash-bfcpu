{-# LANGUAGE RecordWildCards #-}

module UART.FIFO (fifo) where

import CLaSH.Prelude hiding (empty)
--import CLaSH.Signal.Bundle
--import CLaSH.Signal.Internal
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Tuple
import Types

type FifoAddr = Unsigned 8 -- this has to match the size of fifoRam

data FifoState = FifoState
  { _r_ptr  :: FifoAddr
  , _w_ptr  :: FifoAddr
  , _empty  :: Bool
  , _full   :: Bool
  }

makeLenses ''FifoState

-- use 'get' as the last statement in fifoRun results in Verilog with combinatorial loop
--instance Bundle FifoState where
--  type Unbundled' t FifoState =
--    ( Signal' t FifoAddr, Signal' t FifoAddr
--    , Signal' t Bool, Signal' t Bool , Signal' t Bool )
--
--  bundle' _ (a,b,c,d,e) = FifoState <$> a <*> b <*> c <*> d <*> e
--
--  unbundle' _ fifo_state = ( f _r_ptr, f _w_ptr, f _w_en, f _empty, f _full )
--    where
--    f a = a <$> fifo_state

fifoInit :: FifoState
fifoInit = FifoState 0 0 True False

fifoVec :: Vec (2 ^ 8) Data
fifoVec = replicate snat 0

fifoRam :: Signal FifoAddr -> Signal FifoAddr -> Signal Bool -> Signal Data -> Signal Data
--fifoRam  = readNew $ blockRamPow2 fifoVec
fifoRam  = asyncRamPow2

fifoRun :: FifoState -> (Bool, Bool) -> (FifoState, (FifoAddr, FifoAddr, Bool, Bool, Bool))
fifoRun s@(FifoState {..}) input@(rd, wr) = swap $ flip runState s $ do
  case input of
    (True, False) -> -- read
      when (not _empty) $ do
        r_ptr += 1 -- next state, equivalent to `r_ptr_next = r_ptr_succ` in verilog
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
  let w_en = wr && not _full
  return (_r_ptr, _w_ptr, w_en, _empty, _full) -- TODO: better way?

fifo :: Signal Bool -> Signal Bool -> Signal Data -> (Signal Bool, Signal Bool, Signal Data)
fifo rd wr w_data = (empty, full, r_data)
  where
  (r_ptr, w_ptr, w_en, empty, full) = mealyB fifoRun fifoInit (rd, wr)

  r_data = fifoRam w_ptr r_ptr w_en w_data

{-# ANN topEntity
  (defTop
    { t_name     = "fifo"
    , t_inputs   = ["btnL", "btnR", "sw"]
    , t_outputs  = ["led_empty", "led_full", "led"]
    , t_extraIn  = [("clk", 1), ("btnCpuReset", 1)]
    , t_extraOut = []
    , t_clocks   = []
    }) #-}
topEntity = fifo
