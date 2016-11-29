{-# LANGUAGE RecordWildCards #-}

module BF.CPU where

import CLaSH.Prelude
import CLaSH.Signal.Bundle
import CLaSH.Signal.Internal
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
  { _cpu_mode  :: Mode
  , _instr_ptr :: Addr -- 2^16 instructions maximum
  , _instr_reg :: Addr
  , _data_ptr  :: Addr -- 2^16 array cells
  , _data_clr  :: Bool
  }

makeLenses ''BfState

bfInit :: BfState
bfInit = BfState Prog 0 0xFFFF 0 True

-- (rx_done_tick, rx_dout, tx_done_tick, instr_r, data_r)
type CpuIn   = (Bool, Data, Bool, Data, Data)

data CpuOut = CpuOut
  { instr_w_addr :: Addr
  , instr_r_addr :: Addr
  , instr_w_en   :: Bool
  , instr_w      :: Data
  , data_w_addr  :: Addr
  , data_r_addr  :: Addr
  , data_w_en    :: Bool
  , data_w       :: Data
  , prog_mode    :: Bool
  , exec_mode    :: Bool
  , tx_start     :: Bool
  , tx_din       :: Data
  }

instance Bundle CpuOut where
  type Unbundled' t CpuOut =
    ( Signal' t Addr, Signal' t Addr, Signal' t Bool, Signal' t Data
    , Signal' t Addr, Signal' t Addr, Signal' t Bool, Signal' t Data
    , Signal' t Bool, Signal' t Bool, Signal' t Bool, Signal' t Data )

  bundle' _ (a,b,c,d,e,f,g,h,i,j,k,l) = CpuOut <$> a <*> b <*> c <*> d
                                               <*> e <*> f <*> g <*> h
                                               <*> i <*> j <*> k <*> l

  unbundle' _ cpu_out = ( f instr_w_addr, f instr_r_addr, f instr_w_en, f instr_w
                        , f data_w_addr, f data_r_addr, f data_w_en, f data_w
                        , f prog_mode, f exec_mode, f tx_start, f tx_din
                        )
    where
    f a = a <$> cpu_out

defCpuOut :: CpuOut
defCpuOut = CpuOut { instr_w_addr = 0, instr_r_addr = 0, instr_w_en = False, instr_w = 0
                   , data_w_addr = 0, data_r_addr = 0, data_w_en = False, data_w = 0
                   , prog_mode = False, exec_mode = False, tx_start = False, tx_din = 0
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

cpuRun, cpuProgMode, cpuExecMode :: BfState -> CpuIn -> (BfState, CpuOut)
cpuRun s i =
  case _cpu_mode s of
    Prog -> cpuProgMode s i
    Exec -> cpuExecMode s i

cpuProgMode s@(BfState {..}) (rx_done_tick, rx_dout, _, _, _) =
  swap $ flip runState s $
    if _data_clr then do
      if _data_ptr == 2 ^ 16 - 1 then
        data_clr .= False
      else
        data_ptr += 1
      return defCpuOut { data_w_addr = _data_ptr, data_w_en = True, data_w = 0 }
    else do
      let out = defCpuOut { prog_mode = True }
      if rx_done_tick then
        if isInstr rx_dout then do
          instr_ptr += 1 -- TODO: check boundary
          return out { instr_w_addr = _instr_ptr, instr_w_en = True, instr_w = rx_dout }
        else
          if isEOT rx_dout then do
            switch_to_exec
            -- TODO: test the effect of readNew (read before write without it)
            return out { instr_w_addr = _instr_ptr, instr_w_en = True, instr_w = 0 }
          else
            return out
      else
        return out
  where
  switch_to_exec = do
    cpu_mode  .= Exec
    instr_ptr .= 0
    instr_reg .= 0xFFFF -- TODO: weird
    data_ptr  .= 0

cpuExecMode s@(BfState {..}) (rx_done_tick, rx_dout, tx_done_tick, instr_r, data_r) =
  swap $ flip runState s $ do
    if _instr_ptr == _instr_reg then do -- previous instruction hasn't finished
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
        switch_to_prog
        return out'
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
          '[' -> return out'
          ']' -> return out'
          _   -> return out' -- TODO: indicate error
  where
  instr_r' = chr $ fromIntegral instr_r

  out' = defCpuOut { data_r_addr = _data_ptr, exec_mode = True }

  switch_to_prog = do
    cpu_mode  .= Prog
    instr_ptr .= 0
    data_ptr  .= 0
    data_clr  .= True

{-# ANN topEntity
  (defTop
    { t_name     = "bf_cpu"
    , t_inputs   = ["RsRx"]
    , t_outputs  = ["RsTx", "led_prog", "led_exec"]
    , t_extraIn  = [("clk", 1), ("btnCpuReset", 1)]
    , t_extraOut = []
    , t_clocks   = []
    }) #-}
topEntity :: Signal Bit -> (Signal Bit, Signal Bool, Signal Bool)
topEntity rx = (tx, prog_mode, exec_mode)
  where
  (rx_dout, rx_done_tick) = uartRx rx
  (tx, tx_done_tick)      = uartTx tx_start tx_din

  instr_r = instrRam instr_w_addr instr_r_addr instr_w_en instr_w
  data_r  = dataRam data_w_addr data_r_addr data_w_en data_w

  (instr_w_addr, instr_r_addr, instr_w_en, instr_w,
   data_w_addr, data_r_addr, data_w_en, data_w,
   prog_mode, exec_mode, tx_start, tx_din)  = mealyB cpuRun bfInit
      (rx_done_tick, rx_dout,
       tx_done_tick,
       instr_r,
       data_r)
