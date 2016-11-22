{-# LANGUAGE RecordWildCards #-}

module BF.CPU where

import CLaSH.Prelude
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.Tuple
import UART.UART

type Addr = Unsigned 16
type Data = Unsigned 8

data Mode = Prog | Exec

data BfState = BfState
  {
    _cpu_mode  :: Mode
  , _instr_ptr :: Addr -- 2^16 instructions maximum
  , _data_ptr  :: Addr -- 2^16 array cells
  , _is_instr  :: Bool
  , _is_eot    :: Bool
  }

makeLenses ''BfState

bfInit :: BfState
bfInit = BfState Prog 0 0 False False

-- Note: `replicate 65536 0` doesn't work
ramVec :: Vec (2 ^ 16) Data
ramVec = replicate snat 1

-- w_addr -> r_addr -> w_enable -> w_data -> r_data
arrayRam, instrRam :: Signal Addr -> Signal Addr -> Signal Bool -> Signal Data -> Signal Data
arrayRam = blockRamPow2 ramVec
instrRam = blockRamPow2 ramVec

-- "No blackbox found for: GHC.List.elem" if we use List instead of Vec
isInstr :: Data -> Bool
isInstr x = x `elem` (map (fromIntegral . ord) $(v ['>', '<', '+', '-', '.', ',', '[', ']']))

isEOT :: Data -> Bool
isEOT x = x == 4

cpuProgMode :: BfState     -> -- state
               (Bool, Data) -> -- input
               (BfState, (Addr, Addr, Bool, Data, Bool, Bool)) -- (new_state, output)
cpuProgMode s@(BfState {..}) (rx_done_tick, dout) = swap $ flip runState s $ do
  let ptr = _instr_ptr
  if rx_done_tick then
    if isInstr dout then do
      instr_ptr += 1 -- TODO: check boundary
      is_instr .= True
      return (ptr, ptr, True, dout, _is_instr, _is_eot)
    else do
      is_instr .= False
      if isEOT dout then do
        cpu_mode .= Exec
        is_eot .= True
        return (ptr, ptr, False, dout, _is_instr, _is_eot)
      else do
        is_eot .= False
        return (ptr, ptr, False, dout, _is_instr, _is_eot)
  else
    return (ptr, ptr, False, dout, _is_instr, _is_eot)

{-# ANN topEntity
  (defTop
    { t_name     = "bf_cpu"
    , t_inputs   = ["RsRx"]
    , t_outputs  = ["led", "led_instr", "led_eot"]
    , t_extraIn  = [("clk", 1)]
    , t_extraOut = []
    , t_clocks   = []
    }) #-}
topEntity :: Signal Bit -> (Signal Data, Signal Bool, Signal Bool)
topEntity rx = (instr_out, is_instr, is_eot)
  where
  (dout, rx_done_tick) = uartRx rx

  (w_addr, r_addr, w_en, instr_in, is_instr, is_eot) = mealyB cpuProgMode bfInit (rx_done_tick, dout)

  instr_out = instrRam w_addr r_addr w_en instr_in
