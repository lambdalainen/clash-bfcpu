{-# LANGUAGE RecordWildCards #-}

module BF.CPU where

import CLaSH.Prelude
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Tuple
import UART.UART (uart)

data Mode = Prog | Exec

type Addr = Unsigned 16
type Data = BitVector 8

data BfState = BfState
  {
    _cpu_mode  :: Mode
  , _instr_ptr :: Addr -- 2^16 instructions maximum
  , _data_ptr  :: Addr -- 2^16 array cells
  }

makeLenses ''BfState

bfInit :: BfState
bfInit = BfState Prog 0 0

-- Note: `replicate 65536 0` doesn't work
ramVec :: Vec 65536 Data
ramVec = replicate snat 0

-- w_addr -> r_addr -> w_enable -> w_value -> r_value
arrayRam, instrRam :: Signal Addr -> Signal Addr -> Signal Bool -> Signal Data -> Signal Data
arrayRam = blockRamPow2 ramVec
instrRam = blockRamPow2 ramVec

cpuProgMode :: BfState     -> -- state
               (Bit, Data) -> -- input
               (BfState, (Addr, Addr, Bool, Data)) -- (new_state, output)
cpuProgMode s@(BfState {..}) (rx_done_tick, dout) = swap $ flip runState s $ do
  let ptr = _instr_ptr
  if rx_done_tick == 1 then do
    instr_ptr += 1 -- TODO: check boundary
    return (ptr, ptr, True, dout)
  else
    return (ptr, ptr, False, dout)

{-# ANN topEntity
  (defTop
    { t_name     = "bf_cpu"
    , t_inputs   = ["RsRx"]
    , t_outputs  = ["led"]
    , t_extraIn  = [("clk", 1)]
    , t_extraOut = []
    , t_clocks   = []
    }) #-}
topEntity :: Signal Bit -> Signal Data
topEntity rx = instr_out
  where
  (rx_done_tick, dout) = uart rx

  (w_addr, r_addr, w_en, instr_in) = mealyB cpuProgMode bfInit (rx_done_tick, dout)

  instr_out = instrRam w_addr r_addr w_en instr_in
