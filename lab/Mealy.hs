{-# LANGUAGE RecordWildCards #-}

module Mealy where

import CLaSH.Prelude
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Tuple
import Types

data MealyState = MealyState { _mealy_data  :: Data }

makeLenses ''MealyState

mealyInit :: MealyState
mealyInit = MealyState 0

mealyRun :: MealyState -> Bool -> (MealyState, (Data, Data))
mealyRun s@(MealyState {..}) test_view = swap $ flip runState s $ do
  mealy_data += 1
  let even     = _mealy_data `mod` 2 == 0
      view_val = view mealy_data s
  -- first way
  use_val <- use mealy_data
  -- second way
  -- MealyState a <- get
  return (_mealy_data, if test_view then view_val else use_val)

mealyTest :: Signal Bool -> (Signal Data, Signal Data)
mealyTest test_view = mealyB mealyRun mealyInit test_view
