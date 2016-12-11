{-# LANGUAGE RecordWildCards #-}

module BF.CPU where

import CLaSH.Prelude
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.Tuple
import BF.Types
import SSeg.SSeg
import UART.UART

bfInit :: BfState
bfInit = BfState { _cpu_mode = Clear, _instr_ptr = 0, _instr_reg = 0xFFFF , _data_ptr = 0
                 , _search_ptr = 0, _search_for = 0, _search_cnt = 0 }

defCpuOut :: CpuOut
defCpuOut = CpuOut { instr_w_addr = 0, instr_r_addr = 0, instr_w_en = False, instr_w = 0
                   , data_w_addr = 0, data_r_addr = 0, data_w_en = False, data_w = 0
                   , tx_start = False, tx_din = 0, mode = Clear, counter_rst = False
                   }

-- Note: `replicate 65536 0` doesn't work
ramVec :: Vec (2 ^ 16) Data
ramVec = replicate snat 0

-- w_addr -> r_addr -> w_enable -> w_data -> r_data
dataRam, instrRam :: Signal Addr -> Signal Addr -> Signal Bool -> Signal Data -> Signal Data
dataRam  = readNew $ blockRamPow2 ramVec
instrRam = readNew $ blockRamPow2 ramVec

-- "No blackbox found for: GHC.List.elem" if we use List instead of Vec
isInstr :: Data -> Bool
isInstr x = x `elem` (map (fromIntegral . ord) $(v ['>', '<', '+', '-', '.', ',', '[', ']']))

isEOT :: Data -> Bool
isEOT x = x == 4

cpuRun, cpuClearMode, cpuProgMode, cpuExecMode :: BfState -> CpuIn -> (BfState, CpuOut)
cpuRun s i =
  case _cpu_mode s of
    Clear -> cpuClearMode s i
    Prog  -> cpuProgMode  s i
    Exec  -> cpuExecMode  s i

cpuClearMode s@(BfState {..}) _ =
  swap $ flip runState s $ do
    if _data_ptr == 2 ^ 16 - 1 then
      cpu_mode .= Prog
    else
      data_ptr += 1
    return defCpuOut { data_w_addr = _data_ptr, data_w_en = True, data_w = 0 }

cpuProgMode s@(BfState {..}) (rx_done_tick, rx_dout, _, _, _) =
  swap $ flip runState s $ do
    let out = defCpuOut { mode = _cpu_mode }
    if rx_done_tick then
      if isInstr rx_dout then do
        instr_ptr += 1 -- TODO: check boundary
        return out { instr_w_addr = _instr_ptr, instr_w_en = True, instr_w = rx_dout }
      else
        if isEOT rx_dout then do
          switch_to_exec
          -- TODO: test the effect of readNew (read before write without it)
          return out { instr_w_addr = _instr_ptr, instr_w_en = True, instr_w = 0,
                       counter_rst = True }
        else
          return out
    else
      return out
  where
  switch_to_exec = do
    cpu_mode  .= Exec
    instr_ptr .= 0
    instr_reg .= 0xFFFF
    data_ptr  .= 0

cpuExecMode s@(BfState {..}) (rx_done_tick, rx_dout, tx_done_tick, instr_r, data_r) =
  swap $ flip runState s $ do
    if _instr_ptr == _instr_reg then do -- previous instruction hasn't finished
      case data2char _search_for of
        ']' ->
          case instr_r' of
            ']' ->
              if _search_cnt == 0 then do    -- found match
                search_for .= 0              -- stop searching
                instr_ptr .= _search_ptr + 1 -- jump to the instruction after the matching ]
                return out' { instr_r_addr = _search_ptr + 1 }
              else do
                search_cnt -= 1 -- decrease nesting count
                search_ptr += 1
                return out' { instr_r_addr = _search_ptr + 1 }
            '[' -> do
              search_cnt += 1 -- increase nesting count
              search_ptr += 1
              return out' { instr_r_addr = _search_ptr + 1 }
            _   -> do
              search_ptr += 1
              return out' { instr_r_addr = _search_ptr + 1 }
        '[' ->
          case instr_r' of
            '[' ->
              if _search_cnt == 0 then do    -- found match
                search_for .= 0              -- stop searching
                instr_ptr .= _search_ptr + 1 -- jump to the instruction after the matching [
                return out' { instr_r_addr = _search_ptr + 1 }
              else do
                search_cnt -= 1 -- decrease nesting count
                search_ptr -= 1
                return out' { instr_r_addr = _search_ptr - 1 }
            ']' -> do
              search_cnt += 1 -- increase nesting count
              search_ptr -= 1
              return out' { instr_r_addr = _search_ptr - 1 }
            _   -> do
              search_ptr -= 1
              return out' { instr_r_addr = _search_ptr - 1 }
        _   ->
          case instr_r' of
            '.' | tx_done_tick -> do
              instr_ptr += 1
              return out' { instr_r_addr = _instr_ptr + 1 }
            ',' | rx_done_tick -> do
              instr_ptr += 1
              return out' { instr_r_addr = _instr_ptr + 1, data_w_addr = _data_ptr,
                            data_w_en = True, data_w = rx_dout }
            _ -> do
              return out' { instr_r_addr = _instr_ptr }
    else
      if instr_r == 0 then do
        switch_to_clear
        return out' { mode = Clear }
      else do
        instr_reg .= _instr_ptr
        case instr_r' of
          '>' -> do
            instr_ptr += 1
            data_ptr += 1 -- TODO: check boundary
            return out' { instr_r_addr = _instr_ptr + 1, data_r_addr = _data_ptr + 1 }
          '<' -> do
            instr_ptr += 1
            data_ptr -= 1
            return out' { instr_r_addr = _instr_ptr + 1, data_r_addr = _data_ptr - 1 }
          '+' -> do
            instr_ptr += 1
            return out' { instr_r_addr = _instr_ptr + 1, data_w_addr = _data_ptr,
                          data_w_en = True, data_w = data_r + 1 }
          '-' -> do
            instr_ptr += 1
            return out' { instr_r_addr = _instr_ptr + 1, data_w_addr = _data_ptr,
                          data_w_en = True, data_w = data_r - 1 }
          '.' -> do
            return out' { instr_r_addr = _instr_ptr, tx_start = True, tx_din = data_r }
          ',' -> do
            return out' { instr_r_addr = _instr_ptr }
          '[' -> do
            if data_r == 0 then do
              search_ptr .= _instr_ptr + 1
              search_for .= char2data ']'
              search_cnt .= 0
              return out' { instr_r_addr = _instr_ptr + 1 }
            else do
              instr_ptr += 1
              return out' { instr_r_addr = _instr_ptr + 1 }
          ']' -> do
            if data_r /= 0 then do
              search_ptr .= _instr_ptr - 1
              search_for .= char2data '['
              search_cnt .= 0
              return out' { instr_r_addr = _instr_ptr - 1 }
            else do
              instr_ptr += 1
              return out' { instr_r_addr = _instr_ptr + 1 }
          _   -> do
            return out' -- TODO: indicate error
  where
  char2data = fromIntegral . ord
  data2char = chr . fromIntegral

  instr_r' = data2char instr_r

  out' = defCpuOut { data_r_addr = _data_ptr, mode = _cpu_mode }

  switch_to_clear = do
    cpu_mode  .= Clear
    instr_ptr .= 0
    data_ptr  .= 0

clkCounter :: Signal Bool -> Signal Bool -> Signal (Unsigned 32)
clkCounter enable reset = s
  where
  s = regEn 0 (enable .||. reset) ((\rst x -> if rst then 0 else x + 1) <$> reset <*> s)

{-# ANN topEntity
  (defTop
    { t_name     = "bf_cpu"
    , t_inputs   = ["RsRx"]
    , t_outputs  = ["RsTx", "led_prog", "led_exec", "an", "seg"]
    , t_extraIn  = [("clk", 1), ("btnCpuReset", 1)]
    , t_extraOut = []
    , t_clocks   = []
    }) #-}
topEntity :: Signal Bit -> (Signal Bit, Signal Bool, Signal Bool,
                            Signal (Unsigned 8, BitVector 8))
topEntity rx = (tx, is_prog_mode, is_exec_mode, ssegU (clkCounter is_exec_mode counter_rst))
  where
  is_prog_mode = (== Prog) <$> mode
  is_exec_mode = (== Exec) <$> mode

  (rx_dout, rx_done_tick) = uartRx rx
  (tx, tx_done_tick)      = uartTx tx_start tx_din

  instr_r = instrRam instr_w_addr instr_r_addr instr_w_en instr_w
  data_r  = dataRam data_w_addr data_r_addr data_w_en data_w

  (instr_w_addr, instr_r_addr, instr_w_en, instr_w,
   data_w_addr, data_r_addr, data_w_en, data_w,
   tx_start, tx_din, mode, counter_rst) =
     mealyB cpuRun bfInit (rx_done_tick, rx_dout, tx_done_tick,
                           instr_r, data_r)
