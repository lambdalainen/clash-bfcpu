module BF.Types where

import CLaSH.Prelude
import CLaSH.Signal.Bundle
import CLaSH.Signal.Internal
import Control.Lens

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
