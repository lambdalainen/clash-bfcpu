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
  , _instr_reg :: Data
  , _data_ptr  :: Addr -- 2^16 array cells
  , _is_txing  :: Bool -- maybe can be replaced with _instr_reg
  }

makeLenses ''BfState

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

bfInit :: BfState
bfInit = BfState Prog 0 0 0 False

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
  swap $ flip runState s $ do
    let out = defCpuOut { prog_mode = True }
    if rx_done_tick then
      if isInstr rx_dout then do
        instr_ptr += 1 -- TODO: check boundary
        return out { instr_w_addr = _instr_ptr, instr_w_en = True, instr_w = rx_dout }
      else do
        if isEOT rx_dout then do
          switch_to_exec
          return out -- TODO: test the effect of readNew (read before write without it)
        else
          return out
    else
      return out
  where
  switch_to_exec = do
    cpu_mode  .= Exec
    instr_ptr .= 0
    data_ptr  .= 0

cpuExecMode s@(BfState {..}) (rx_done_tick, rx_dout, tx_done_tick, instr_r, data_r) =
  swap $ flip runState s $ do
    if _is_txing then do
      when tx_done_tick $
        is_txing .= False
      return out'
    else
      if instr_r == 0 then do
        switch_to_prog
        return out'
      else do
        case instr_r' of
          '>' -> data_ptr += 1
          '<' -> data_ptr -= 1
          '+' -> return ()
          '-' -> return ()
          '.' -> return ()
          ',' -> return ()
          '[' -> return ()
          ']' -> return ()
          _   -> return ()
        is_txing .= True
        instr_ptr += 1
        return out' { tx_start = True, tx_din = instr_r }
  where
  instr_r' = chr $ fromIntegral instr_r

  out' = defCpuOut { instr_r_addr = _instr_ptr, exec_mode = True }

  out  = case instr_r' of
    '>' -> out' { data_w_addr = _data_ptr, data_r_addr = _data_ptr }
    '<' -> out' { data_w_addr = _data_ptr, data_r_addr = _data_ptr }
    '+' -> out' { data_w_en = True, data_w = data_r + 1 }
    '-' -> out' { data_w_en = True, data_w = data_r - 1 }
    '.' -> out' { tx_start = True, tx_din = data_r }
    ',' -> out' { data_w_addr = _data_ptr, data_w = rx_dout }
    '[' -> out'
    ']' -> out'
    _   -> out' -- TODO: indicate error

  switch_to_prog = do
    cpu_mode  .= Prog
    instr_ptr .= 0
    data_ptr  .= 0

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
