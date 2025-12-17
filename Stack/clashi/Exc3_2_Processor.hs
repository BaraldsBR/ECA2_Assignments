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
module Exc3_2_Processor where

import Clash.Prelude
import Clash.Signal
import qualified Exc2_2_Stack as Stack

import qualified Data.List as L

type Value = Stack.Value

data Opcode = Mult | Add
  deriving (Show, Generic, NFDataX)

data Instr = Push Value | Calc Opcode | Nop
  deriving (Show, Generic, NFDataX)

data State = Calcing (Maybe Value) Opcode | Idle
  deriving (Show, Generic, NFDataX)

processor :: State -> (Instr, Value) -> (State, (Stack.Instr, Maybe Value))
processor state (instr, value) = undefined -- extend your definition



procBlock :: HiddenClockResetEnable dom
  => Signal dom (Instr, Value) -> Signal dom (Stack.Instr, Maybe Value)
procBlock = undefined -- extend your definition



system :: HiddenClockResetEnable dom
  => Signal dom Instr -> Signal dom (Maybe Value)
system instr = undefined -- extend your definition


testSystem = simulateN @System len system inp
  where
    inp = [Nop, Nop, Push 3, Push 6, Calc Add, Nop, Nop, Push 2, Calc Mult, Nop, Nop, Nop, Calc Mult, Nop, Nop]
    len = L.length inp
