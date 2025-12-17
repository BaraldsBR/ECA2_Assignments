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
module Exc4_Fetch where

import Clash.Prelude
import Clash.Signal
import Clash.Sized.Vector

import qualified Exc2_2_Stack as Stack
import qualified Exc3_2_Processor as Proc
import qualified Data.List as L

import Debug.Trace

type Address = Unsigned 8
type Program = Vec 256 Proc.Instr
type StallTimer = Unsigned 2

type State = (Address, StallTimer) -- (Program Counter, stall countdown state variable)

type Output = (Proc.Instr, Address) -- (Instruction for the processor, address for the instruction BRAM)

testProgram :: Program
testProgram =
    (Proc.Push 2):>
    (Proc.Push 10):>
    (Proc.Calc Proc.Mult):>
    (Proc.Push 3):>
    (Proc.Push 4):>
    (Proc.Push 11):>
    (Proc.Calc Proc.Add):>
    (Proc.Calc Proc.Mult):>
    (Proc.Calc Proc.Add):>
    (Proc.Push 12):>
    (Proc.Push 5):>
    (Proc.Calc Proc.Add):>
    (Proc.Calc Proc.Mult):>
    (Proc.Nop):>Nil
    ++
    repeat Proc.Nop

fetcher :: State -> Proc.Instr -> (State, Output)
fetcher (pc, stallt) instr = undefined -- add your definition


instrBRAM :: HiddenClockResetEnable dom =>
    Signal dom Address -> Signal dom (Maybe (Address, Proc.Instr)) -> Signal dom Proc.Instr
instrBRAM = blockRam $ testProgram


fetchBlock :: HiddenClockResetEnable dom =>
    Signal dom Proc.Instr -> Signal dom Output
fetchBlock = undefined -- add your definition


system :: HiddenClockResetEnable dom => Signal dom Proc.Instr
system = undefined -- add your definition


testSystem = mapM_ print $ sampleN @System 32 system
