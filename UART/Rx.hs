{-# LANGUAGE RecordWildCards #-}

module UART.Rx (rxInit, rxRun) where

import CLaSH.Prelude
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Tuple
import Types

data RxState = RxState
  { _rx_done_tick :: Bool
  , _rx_dout      :: Data       -- byte register (same as _b_reg in Tx)
  , _rx_state     :: Unsigned 2
  , _s_reg        :: Unsigned 4 -- sampling counter
  , _n_reg        :: Unsigned 3 -- number of bits received
  }

makeLenses ''RxState

rxInit :: RxState
rxInit = RxState False 0 0 0 0

rxRun :: RxState -> (Bit, Bit) -> (RxState, (Data, Bool))
rxRun s@(RxState {..}) (rx, s_tick) = swap $ flip runState s $ do
  rx_done_tick .= False
  case _rx_state of
    0 -> idle
    1 -> start
    2 -> rdata
    3 -> stop
  done_tick <- use rx_done_tick
  return (_rx_dout, done_tick)
  where
  idle  = when (rx == 0) $ do
            rx_state .= 1
            s_reg .= 0

  start = when (s_tick == 1) $
            if _s_reg == 7 then do
              rx_state .= 2
              s_reg .= 0
              n_reg .= 0
            else
              s_reg += 1

  rdata = when (s_tick == 1) $
            if _s_reg == 15 then do
              s_reg .= 0
              rx_dout %= replaceBit _n_reg rx
              if _n_reg == 7 then -- 8 bits
                rx_state .= 3
              else
                n_reg += 1
            else
              s_reg += 1

  stop  = when (s_tick == 1) $
            if _s_reg == 15 then do
                rx_state .= 0
                rx_done_tick .= True
            else
              s_reg += 1
