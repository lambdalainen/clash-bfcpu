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

-- TODO: question: what if there are two Test-arguments?
