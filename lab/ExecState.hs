-- Run with `stack exec -- ghci`
{-# LANGUAGE TemplateHaskell #-}
-- At first we might wonder, can on earch can we write 'when (_test < 10)'
-- where '_test' needs an argument.
--
-- This RecordWildCards is the key!
{-# LANGUAGE RecordWildCards #-}

module ExecState where

-- Prelude> :l ExecState.hs 
-- [1 of 1] Compiling ExecState        ( ExecState.hs, interpreted )
-- Ok, modules loaded: ExecState.
-- *ExecState> let t = Test 0
-- *ExecState> test_state t
-- Test {_test = 1}

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State

data Test = Test { _test :: Int }
  deriving Show

makeLenses ''Test

-- RecordWildCards enables us to write '_test < 10', while '+=' is made for
-- MonadState which enables us to write 'test += 1'
test_state t@(Test {..}) = flip execState t $ when (_test < 10) $ test += 1

test_state2 t@(Test {..}) = flip runState t $ do
  let out = _test -- although out is lazily evaluated, the output will be the value before update
  test += 1
  return out

data CpuOut = CpuOut { addr :: Int }
  deriving Show

defCpuOut = CpuOut { addr = 0 }

-- This test shows that '_test' is captured in 'out' once t is evaluated, just as in test_state2
test_state3 t@(Test {..}) n = flip runState t $ do
  case n of
    0 -> test += 1
    1 -> test += 2
    2 -> test += 3
    _ -> test += 4
  return out
  where
  out' = defCpuOut

  out = case n of
    0 -> out' { addr = _test }
    1 -> out' { addr = _test }
    2 -> out' { addr = _test }
    _ -> out' { addr = _test }

test_state4 t@(Test {..}) n = flip runState t $ do
  case n of
    0 -> test += 1
    1 -> test += 2
    2 -> test += 3
    _ -> test += 4
  return out' { addr = _test } -- even this doesn't work, since _test means '_test t'
  where
  out' = defCpuOut

test_state5 t@(Test {..}) n = flip runState t $ do
  case n of
    0 -> test += 1
    1 -> test += 2
    2 -> test += 3
    _ -> test += 4
  new <- use test -- this works
  return out' { addr = new }
  where
  out' = defCpuOut

-- TODO: question: what if there are two Test-arguments?
