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
  , _instr_reg :: Data
  , _data_ptr  :: Addr -- 2^16 array cells
  , _is_txing  :: Bool
  }

makeLenses ''BfState

bfInit :: BfState
bfInit = BfState Prog 0 0 0 False

-- Note: `replicate 65536 0` doesn't work
ramVec :: Vec (2 ^ 16) Data
ramVec = replicate snat 0

-- w_addr -> r_addr -> w_enable -> w_data -> r_data
arrayRam, instrRam :: Signal Addr -> Signal Addr -> Signal Bool -> Signal Data -> Signal Data
arrayRam = blockRamPow2 ramVec
instrRam = blockRamPow2 ramVec

-- "No blackbox found for: GHC.List.elem" if we use List instead of Vec
isInstr :: Data -> Bool
isInstr x = x `elem` (map (fromIntegral . ord) $(v ['>', '<', '+', '-', '.', ',', '[', ']']))

isEOT :: Data -> Bool
isEOT x = x == 4

cpuRun, cpuProgMode, cpuExecMode ::
  BfState                  -> -- state
  (Bool, Data, Bool, Data) -> -- input
  (BfState, (Addr, Addr, Bool, Data, Bool, Bool, Bool, Data)) -- (new_state, output)
cpuRun s i =
  case _cpu_mode s of
    Prog -> cpuProgMode s i
    Exec -> cpuExecMode s i

cpuProgMode s@(BfState {..}) (rx_done_tick, dout, _, _) =
  swap $ flip runState s $ do
    let out = (_instr_ptr, 0, False, dout, True, False, False, 0)
    if rx_done_tick then
      if isInstr dout then do
        instr_ptr += 1 -- TODO: check boundary
        return (_instr_ptr, 0, True, dout, True, False, False, 0)
      else do
        if isEOT dout then do
          switch_to_exec
          return out
        else
          return out
    else
      return out
  where
  switch_to_exec = do
    cpu_mode  .= Exec
    instr_ptr .= 0
    data_ptr  .= 0

cpuExecMode s@(BfState {..}) (rx_done_tick, dout, tx_done_tick, instr_out) =
  swap $ flip runState s $ do
    let ptr = _instr_ptr
    if _is_txing then do
      when tx_done_tick $
        is_txing .= False
      return (0, ptr, False, dout, False, True, False, 0)
    else do
      if instr_out == 0 then
        return (0, ptr, False, dout, False, True, False, instr_out)
      else do
        is_txing .= True
        instr_ptr += 1
        return (0, ptr, False, dout, False, True, True, instr_out)

{-# ANN topEntity
  (defTop
    { t_name     = "bf_cpu"
    , t_inputs   = ["RsRx"]
    , t_outputs  = ["RsTx", "led_prog", "led_exec"]
    , t_extraIn  = [("clk", 1)]
    , t_extraOut = []
    , t_clocks   = []
    }) #-}
topEntity :: Signal Bit -> (Signal Bit, Signal Bool, Signal Bool)
topEntity rx = (tx, prog_mode, exec_mode)
  where
  (dout, rx_done_tick) = uartRx rx

  (tx, tx_done_tick)   = uartTx tx_start tx_data

  (w_addr, r_addr, w_en, instr_in, prog_mode, exec_mode, tx_start, tx_data)  =
    mealyB cpuRun bfInit (rx_done_tick, dout, tx_done_tick, instr_out)

  instr_out = instrRam w_addr r_addr w_en instr_in
