module BF.Types where

import CLaSH.Prelude
import CLaSH.Signal.Bundle
import CLaSH.Signal.Internal
import Control.Lens

type Addr = Unsigned 16
type Data = Unsigned 8

data Mode = Clear | Prog | Exec
  deriving Eq

data BfState = BfState
  { _cpu_mode   :: Mode
  , _instr_ptr  :: Addr -- 2^16 instructions maximum
  , _instr_reg  :: Addr
  , _data_ptr   :: Addr -- 2^16 array cells
  , _search_ptr :: Addr
  , _search_for :: Data
  , _search_cnt :: Data -- this could limit our nesting [] levels to 2^8
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
  , tx_start     :: Bool
  , tx_din       :: Data
  , mode         :: Mode
  }

instance Bundle CpuOut where
  type Unbundled' t CpuOut =
    ( Signal' t Addr, Signal' t Addr, Signal' t Bool, Signal' t Data
    , Signal' t Addr, Signal' t Addr, Signal' t Bool, Signal' t Data
    , Signal' t Bool, Signal' t Data, Signal' t Mode )

  bundle' _ (a,b,c,d,e,f,g,h,i,j,k) = CpuOut <$> a <*> b <*> c <*> d
                                             <*> e <*> f <*> g <*> h
                                             <*> i <*> j <*> k

  unbundle' _ cpu_out = ( f instr_w_addr, f instr_r_addr, f instr_w_en, f instr_w
                        , f data_w_addr, f data_r_addr, f data_w_en, f data_w
                        , f tx_start, f tx_din, f mode
                        )
    where
    f a = a <$> cpu_out
