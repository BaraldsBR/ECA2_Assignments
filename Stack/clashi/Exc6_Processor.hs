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
module Exc6_Processor where
-- The NOINLINE options are here because of synthesis later on.

import Clash.Prelude
import Clash.Signal
import qualified Exc2_2_Stack as Stack
import qualified Data.List as L
import Debug.Trace

type Value = Stack.Value

type RegisterFile = Vec 4 Value
type RegisterAddress = Unsigned 2

data Opcode = Mult | Add
    deriving (Show, Generic, NFDataX)

data Instr
  = Push Value
  | Calc Opcode
  | PushR RegisterAddress
  | Save RegisterAddress
  | Nop
  deriving (Show, Generic, NFDataX)

type State = (ProcState, RegisterFile)

data ProcState 
  = Calcing (Maybe Value) Opcode
  | Saving RegisterAddress
  | Idle
  deriving (Show, Generic, NFDataX)

processor :: State -> (Instr, Value) -> (State, (Stack.Instr, Value))
processor (state, regs) (instr, value) = case state of
  Idle -> case instr of
    Push v      -> ((Idle, regs), (Stack.Push v, regs !! 0))
    PushR addr  -> ((Idle, regs), (Stack.Push $ regs !! addr, regs !! 0))
    Save addr   -> ((Saving addr, regs), (Stack.Pop, regs !! 0))
    Calc opCode -> ((Calcing Nothing opCode, regs), (Stack.Pop, regs !! 0))
    _           -> ((Idle, regs), (Stack.Nop, regs !! 0))
  Calcing storedValue opCode -> output 
    where
      operation = case opCode of
        Add  -> (+)
        Mult -> (*)
      output = case storedValue of
        Just previous -> ((Idle, regs), (Stack.Push opResult, regs !! 0)) where opResult = operation value previous
        Nothing       -> ((Calcing (Just value) opCode, regs), (Stack.Pop, regs !! 0))
  Saving addr -> ((Idle, newRegs), (Stack.Nop, regs !! 0)) where newRegs = replace addr value regs


{-# NOINLINE procBlock #-}
procBlock :: HiddenClockResetEnable dom
  => Signal dom (Instr, Value) -> Signal dom (Stack.Instr, Value)
procBlock = mealy processor (Idle, replicate d4 5)


{-# NOINLINE system #-}
system :: HiddenClockResetEnable dom
  => Signal dom Instr -> Signal dom Value
system instr = output
  where
    readResult = Stack.system stackInstruction
    (stackInstruction, output) = unbundle $ procBlock $ bundle (instr, readResult) -- repeat from 3.2, still looks bad


{-# NOINLINE system' #-}
system' :: HiddenClockResetEnable dom
  => RegisterFile -> Signal dom Instr -> Signal dom Value
system' regs instr = output
  where
    procBlock' = mealy processor (Idle, regs)
    readResult = Stack.system stackInstruction
    (stackInstruction, output) = unbundle $ procBlock' $ bundle (instr, readResult) 





testSystem = simulateN @System len system inp
  where
    inp = [ Nop, Nop, PushR 0, Push 3, Calc Add, Nop, Nop, Save 1
          , Nop, PushR 2, Push 2, Calc Mult, Nop, Nop, Save 3, Nop
          , PushR 3, PushR 1, Calc Add, Nop, Nop, Save 0, Nop, Nop ]
    len = L.length inp


testSystem' = simulateN @System len (system' (1:>3:>5:>7:>Nil)) inp
  where
    inp = [ Nop, Nop, PushR 0, Push 3, Calc Mult, Nop, Nop, Save 0, Nop
          , PushR 0, PushR 1, Calc Add, Nop, Nop, Save 1, Nop
          , PushR 1, PushR 3, Calc Add, Nop, Nop, Save 3, Nop
          , PushR 2, PushR 3, Calc Add, Nop, Nop, Save 0, Nop, Nop]
    len = L.length inp

