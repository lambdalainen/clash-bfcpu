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

-- _instr_w_en and _data_w_en are True for Clear mode
bfInit :: BfState
bfInit = BfState { _cpu_mode = Clear, _instr_ptr = 0, _instr_reg = 0xFFFF , _instr_w_en = True
                 , _instr_w = 0, _data_ptr = 0, _data_w_en = True, _data_w = 0
                 , _search_for = 0, _search_cnt = 0 , _tx_start = False, _tx_din = 0
                 , _counter_rst = False }

-- Note: `replicate 65536 0` doesn't work
ramVec :: Vec (2 ^ 16) Data
ramVec = replicate snat 0

-- TODO: test the effect of readNew (read before write without it)
-- w_addr -> r_addr -> w_enable -> w_data -> r_data
dataRam, instrRam :: Signal Addr -> Signal Addr -> Signal Bool -> Signal Data -> Signal Data
dataRam  = readNew $ blockRamPow2 ramVec
instrRam = readNew $ blockRamPow2 ramVec

-- "No blackbox found for: GHC.List.elem" if we use List instead of Vec
isInstr :: Data -> Bool
isInstr x = x `elem` (map (fromIntegral . ord) $(v ['>', '<', '+', '-', '.', ',', '[', ']']))

isEOT :: Data -> Bool
isEOT x = x == 4

cpuRun :: BfState -> CpuIn -> (BfState, CpuOut)
cpuRun s@BfState { _cpu_mode = a } i =
  (s', (_instr_ptr, _instr_w_en, _instr_w,
        _data_ptr, _data_w_en, _data_w,
        _tx_start, _tx_din, _cpu_mode, _counter_rst))
  where
  s'@BfState {..} = case a of
                      Clear -> cpuClearMode s i
                      Prog  -> cpuProgMode  s i
                      Exec  -> cpuExecMode  s i

--cpuClearMode, cpuProgMode, cpuExecMode :: BfState -> CpuIn -> (BfState, CpuOut)
cpuClearMode s@(BfState {..}) _ =
  flip execState s $ do
    if _data_ptr == 2 ^ 16 - 1 then do
      cpu_mode .= Prog
      instr_ptr .= 0xFFFF -- although it should be this value already, set it explicitly
      instr_w_en .= False
      data_w_en .= False
    else do
      instr_ptr += 1
      data_ptr += 1

cpuProgMode s@(BfState {..}) (rx_done_tick, rx_dout, _, _, _) =
  flip execState s $ do
    instr_w_en .= False
    when rx_done_tick $ do
      if isInstr rx_dout then do
        instr_ptr += 1 -- TODO: check boundary
        instr_w_en .= True
        instr_w .= rx_dout
      else
        when (isEOT rx_dout) $ do
          cpu_mode .= Exec
          instr_ptr .= 0
          instr_reg .= 0xFFFF
          data_ptr .= 0
          counter_rst .= True -- reset clock counter

cpuExecMode s@(BfState {..}) (rx_done_tick, rx_dout, tx_full, instr_r, data_r) =
  flip execState s $ do
    instr_w_en .= False
    data_w_en .= False
    tx_start .= False
    counter_rst .= False
    case data2char _search_for of
      ']' ->
        case instr_r' of
          ']' ->
            if _search_cnt == 0 then do -- found match
              search_for .= 0           -- stop searching
              instr_ptr += 1            -- jump to the instruction after the matching ]
            else do
              search_cnt -= 1 -- decrease nesting count
              instr_ptr += 1
          '[' -> do
            search_cnt += 1 -- increase nesting count
            instr_ptr += 1
          _ ->
            instr_ptr += 1
      '[' ->
        case instr_r' of
          '[' ->
            if _search_cnt == 0 then do -- found match
              search_for .= 0           -- stop searching
              instr_ptr += 1            -- jump to the instruction after the matching [
            else do
              search_cnt -= 1 -- decrease nesting count
              instr_ptr -= 1
          ']' -> do
            search_cnt += 1 -- increase nesting count
            instr_ptr -= 1
          _ ->
            instr_ptr -= 1
      _   -> -- not searching
        if _instr_ptr == _instr_reg then do -- previous instruction hasn't finished
          case instr_r' of
            '.' | not tx_full -> do
                instr_ptr += 1
                tx_start .= True
                tx_din .= data_r
            ',' | rx_done_tick -> do
              instr_ptr += 1
              data_w_en .= True
              data_w .= rx_dout
            _ -> return ()
        else
          if instr_r == 0 then
            modify $ const bfInit
          else do
            instr_reg .= _instr_ptr
            case instr_r' of
              '>' -> do
                instr_ptr += 1
                data_ptr += 1 -- TODO: check boundary
              '<' -> do
                instr_ptr += 1
                data_ptr -= 1
              '+' -> do
                instr_ptr += 1
                data_w_en .= True
                data_w .= data_r + 1
              '-' -> do
                instr_ptr += 1
                data_w_en .= True
                data_w .= data_r - 1
              '.' -> do
                when (not tx_full) $ do
                  instr_ptr += 1
                  tx_start .= True
                  tx_din .= data_r
              ',' -> return ()
              '[' -> do
                if data_r == 0 then do
                  instr_ptr += 1
                  search_for .= char2data ']'
                  search_cnt .= 0
                else
                  instr_ptr += 1
              ']' -> do
                if data_r /= 0 then do
                  instr_ptr -= 1
                  search_for .= char2data '['
                  search_cnt .= 0
                else
                  instr_ptr += 1
              _   -> return ()
  where
  char2data = fromIntegral . ord
  data2char = chr . fromIntegral

  instr_r' = data2char instr_r

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
  (tx, tx_full)           = uartTx tx_start tx_din

  instr_r = instrRam instr_ptr instr_ptr instr_w_en instr_w
  data_r  = dataRam data_ptr data_ptr data_w_en data_w

  cpu_in  = (rx_done_tick, rx_dout, tx_full, instr_r, data_r)

  (instr_ptr, instr_w_en, instr_w,
   data_ptr, data_w_en, data_w,
   tx_start, tx_din, mode, counter_rst) = mealyB cpuRun bfInit cpu_in
