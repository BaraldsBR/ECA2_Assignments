{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}
{-
Student information:
  Student 1
    lastname:
    student number:
  Student 2
    lastname:
    student number:
-}
module Exc3_1_Processor where

import Clash.Prelude
import Clash.Signal
import qualified Exc2_2_Stack as Stack

import qualified Data.List as L

type Value = Stack.Value

data Instr = Push Value
  deriving (Show, Generic, NFDataX)


processor :: () -> (Instr, Value) -> ((), Stack.Instr)
processor state (instr, value) = undefined -- add your definition


procBlock :: HiddenClockResetEnable dom
  => Signal dom (Instr, Value) -> Signal dom (Stack.Instr)
procBlock = undefined -- add your definition


system :: HiddenClockResetEnable dom
  => Signal dom Instr -> Signal dom Value
system instr = undefined -- add your definition


testSystem = mapM_ print $ simulateN @System len system inp
  where
    inp = [Push 1, Push 2, Push 3, Push 4, Push 5, Push 6, Push 7, Push 8, Push 9, Push 10, Push 11]
    len = L.length inp
