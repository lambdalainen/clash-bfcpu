{-# LANGUAGE RecordWildCards #-}

module UART.Tx (TxState(_tx, _tx_done_tick), txInit, txRun) where

import CLaSH.Prelude
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State

data TxState = TxState
  { _tx           :: Bit
  , _tx_done_tick :: Bit
  , _tx_state     :: Unsigned 2
  , _s_reg        :: Unsigned 4 -- sampling counter
  , _n_reg        :: Unsigned 3 -- number of bits received
  , _b_reg        :: Unsigned 8 -- byte register
  }

makeLenses ''TxState

txInit :: TxState
txInit = TxState 1 0 0 0 0 0

txRun :: TxState -> Bit -> Bit -> Unsigned 8 -> TxState
txRun s@(TxState {..}) tx_start s_tick din = flip execState s $ do
  tx_done_tick .= 0
  case _tx_state of
    0 -> idle
    1 -> start
    2 -> rdata
    3 -> stop
  where
  idle  = do
    tx .= 1
    when (tx_start == 1) $ do
      tx_state .= 1
      s_reg .= 0
      b_reg .= din

  start = do
    tx .= 0
    when (s_tick == 1) $
      if _s_reg == 15 then do
        tx_state .= 2
        s_reg .= 0
        n_reg .= 0
      else
        s_reg += 1

  rdata = do
    tx .= _b_reg ! 0
    when (s_tick == 1) $
      if _s_reg == 15 then do
        s_reg .= 0
        b_reg .= _b_reg `shiftR` 1
        if _n_reg == 7 then -- 8 bits
          tx_state .= 3
        else
          n_reg += 1
      else
        s_reg += 1

  stop  = do
    tx .= 1
    when (s_tick == 1) $
      if _s_reg == 15 then do
          tx_state .= 0
          tx_done_tick .= 1
      else
        s_reg += 1
