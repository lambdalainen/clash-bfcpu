{-# LANGUAGE RecordWildCards #-}
module UART (uart) where

import CLaSH.Prelude
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State

-- TODO:
-- Question: how does the use of State monad and lenses influence the resulting Verilog?

-- UART RX Logic
data RxReg
  = RxReg
  { _rx_reg        :: BitVector 8
  , _rx_data       :: BitVector 8
  , _rx_sample_cnt :: Unsigned 4
  , _rx_cnt        :: Unsigned 4
  , _rx_frame_err  :: Bool
  , _rx_over_run   :: Bool
  , _rx_empty      :: Bool
  , _rx_d1         :: Bit
  , _rx_d2         :: Bit
  , _rx_busy       :: Bool
  }

makeLenses ''RxReg

-- execState :: State s a -> s -> s 
-- Evaluate a state computation with the given initial state and return the final state,
-- discarding the final value.

-- The RecordWildCards is the reason we don't have to write '_tx_cnt t'

uartRX r@(RxReg {..}) rx_in uld_rx_data rx_enable = flip execState r $ do
  -- Synchronise the async signal
  rx_d1 .= rx_in
  rx_d2 .= _rx_d1
  -- Uload the rx data
  when uld_rx_data $ do
    rx_data  .= _rx_reg
    rx_empty .= True
  -- Receive data only when rx is enabled
  if rx_enable then do
    -- Check if just received start of frame
    when (not _rx_busy && _rx_d2 == 0) $ do
      rx_busy       .= True
      rx_sample_cnt .= 1
      rx_cnt        .= 0
    -- Star of frame detected, Proceed with rest of data
    when _rx_busy $ do
      rx_sample_cnt += 1
      -- Logic to sample at middle of data
      when (_rx_sample_cnt == 7) $ do
        if _rx_d1 == 1 && _rx_cnt == 0 then
          rx_busy .= False
        else do
          rx_cnt += 1
          -- start storing the rx data
          when (_rx_cnt > 0 && _rx_cnt < 9) $ do
            rx_reg %= replaceBit (_rx_cnt - 1) _rx_d2
          when (_rx_cnt == 9) $ do
            rx_busy .= False
            -- Check if End of frame received correctly
            if _rx_d2 == 0 then
              rx_frame_err .= True
            else do
              rx_empty     .= False
              rx_frame_err .= False
              -- Check if last rx data was not unloaded
              rx_over_run  .= not _rx_empty
  else do
    rx_busy .= False

-- UART TX Logic
data TxReg
  = TxReg
  { _tx_reg      :: BitVector 8
  , _tx_empty    :: Bool
  , _tx_over_run :: Bool
  , _tx_out      :: Bit
  , _tx_cnt      :: Unsigned 4
  }

makeLenses ''TxReg

uartTX t@(TxReg {..}) ld_tx_data tx_data tx_enable = flip execState t $ do
  when ld_tx_data $ do
    if not _tx_empty then
      tx_over_run .= False
    else do
      tx_reg   .= tx_data
      tx_empty .= False
  when (tx_enable && not _tx_empty) $ do
    tx_cnt += 1
    when (_tx_cnt == 0) $
      tx_out .= 0
    when (_tx_cnt > 0 && _tx_cnt < 9) $
      tx_out .= _tx_reg ! (_tx_cnt - 1)
    when (_tx_cnt == 9) $ do
      tx_out   .= 1
      tx_cnt   .= 0
      tx_empty .= True
  unless tx_enable $
    tx_cnt .= 0

-- Combine RX and TX logic
uart ld_tx_data tx_data tx_enable rx_in uld_rx_data rx_enable =
    ( _tx_out   <$> txReg
    , _tx_empty <$> txReg
    , _rx_data  <$> rxReg
    , _rx_empty <$> rxReg
    )
  where
    rxReg     = register rxRegInit (uartRX <$> rxReg <*> rx_in <*> uld_rx_data
                                           <*> rx_enable)
    rxRegInit = RxReg { _rx_reg        = 0
                      , _rx_data       = 0
                      , _rx_sample_cnt = 0
                      , _rx_cnt        = 0
                      , _rx_frame_err  = False
                      , _rx_over_run   = False
                      , _rx_empty      = True
                      , _rx_d1         = 1
                      , _rx_d2         = 1
                      , _rx_busy       = False
                      }

    txReg     = register txRegInit (uartTX <$> txReg <*> ld_tx_data <*> tx_data
                                           <*> tx_enable)
    txRegInit = TxReg { _tx_reg      = 0
                      , _tx_empty    = True
                      , _tx_over_run = False
                      , _tx_out      = 1
                      , _tx_cnt      = 0
                      }
