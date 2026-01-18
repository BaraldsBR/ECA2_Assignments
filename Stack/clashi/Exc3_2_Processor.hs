{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}
{-
Student information:
  Student 1
    lastname: Souza
    student number: s3751163
  Student 2
    lastname: Daskalov
    student number: s2150883
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
processor state (instr, value) = case state of
  Idle -> case instr of
    Push v  -> (Idle, (Stack.Push v, Nothing))
    Calc opCode -> (Calcing Nothing opCode, (Stack.Pop, Nothing))
    _       -> (Idle, (Stack.Nop, Nothing))
  Calcing storedValue opCode -> output 
    where
      operation = case opCode of
        Add  -> (+)
        Mult -> (*)
      output = case storedValue of
        Just previous -> (Idle, (Stack.Push opResult, (Just opResult))) where opResult = operation value previous
        Nothing       -> (Calcing (Just value) opCode, (Stack.Pop, Nothing))
    
{-
  I assume the processor should only take a new instruction if it is Idle,
  that is why the state check comes before the instruction check.
  Update: confirmed solution on Exercise 4
-}


procBlock :: HiddenClockResetEnable dom
  => Signal dom (Instr, Value) -> Signal dom (Stack.Instr, Maybe Value)
procBlock = mealy processor Idle



system :: HiddenClockResetEnable dom
  => Signal dom Instr -> Signal dom (Maybe Value)
system instr = opResult
  where
    readResult = Stack.system stackInstruction
    (stackInstruction, opResult) = unbundle $ procBlock $ bundle (instr, readResult) -- this looks bad but I think it is correct (?)


testSystem = simulateN @System len system inp
  where
    inp = [Nop, Nop, Push 3, Push 6, Calc Add, Nop, Nop, Push 2, Calc Mult, Nop, Nop, Nop, Calc Mult, Nop, Nop]
    len = L.length inp
