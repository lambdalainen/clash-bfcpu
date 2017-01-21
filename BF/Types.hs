module BF.Types
  ( module BF.Types
  , module Types ) where

import CLaSH.Prelude
import CLaSH.Signal.Bundle
import CLaSH.Signal.Internal
import Control.Lens
import Types

data Mode = Clear | Prog | Exec
  deriving Eq

data BfState = BfState
  { _cpu_mode    :: Mode
  , _instr_ptr   :: Addr -- 2^16 instructions maximum
  , _instr_reg   :: Addr
  , _instr_w_en  :: Bool
  , _instr_w     :: Data
  , _data_ptr    :: Addr -- 2^16 array cells
  , _data_w_en   :: Bool
  , _data_w      :: Data
  , _search_for  :: Data
  , _search_cnt  :: Data -- this could limit our nesting [] levels to 2^8
  , _tx_start    :: Bool
  , _tx_din      :: Data
  , _counter_rst :: Bool
  }

makeLenses ''BfState

-- (rx_done_tick, rx_dout, tx_full, instr_r, data_r)
type CpuIn   = (Bool, Data, Bool, Data, Data)

type CpuOut  = (Addr, Bool, Data, Addr, Bool, Data, Bool, Data, Mode, Bool)

-- TODO: how to auto-generate this?
instance Bundle (a,b,c,d,e,f,g,h,i,j) where
  type Unbundled' t (a,b,c,d,e,f,g,h,i,j) =
    ( Signal' t a, Signal' t b, Signal' t c, Signal' t d
    , Signal' t e, Signal' t f, Signal' t g, Signal' t h
    , Signal' t i, Signal' t j)

  bundle'   _ (a,b,c,d,e,f,g,h,i,j) =
    (,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j

  unbundle' _ tup = (fmap (\(x,_,_,_,_,_,_,_,_,_) -> x) tup
                    ,fmap (\(_,x,_,_,_,_,_,_,_,_) -> x) tup
                    ,fmap (\(_,_,x,_,_,_,_,_,_,_) -> x) tup
                    ,fmap (\(_,_,_,x,_,_,_,_,_,_) -> x) tup
                    ,fmap (\(_,_,_,_,x,_,_,_,_,_) -> x) tup
                    ,fmap (\(_,_,_,_,_,x,_,_,_,_) -> x) tup
                    ,fmap (\(_,_,_,_,_,_,x,_,_,_) -> x) tup
                    ,fmap (\(_,_,_,_,_,_,_,x,_,_) -> x) tup
                    ,fmap (\(_,_,_,_,_,_,_,_,x,_) -> x) tup
                    ,fmap (\(_,_,_,_,_,_,_,_,_,x) -> x) tup
                    )
