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
module Exc7_Fetch where

import Clash.Prelude
import Clash.Signal
import Clash.Sized.Vector

import qualified Exc2_2_Stack as Stack
import qualified Exc6_Processor as Proc
import Exc6_Processor (Instr (Push, PushR, Save, Nop, Calc), Opcode (Mult, Add))
import qualified Data.List as L

type Address = Unsigned 4
type Program = Vec 16 Proc.Instr
type StallTimer = Unsigned 2

type State = (Address, StallTimer) -- (Program Counter, stall countdown state variable)

type Output = (Proc.Instr, Address) -- (Instruction for the processor, address for the instruction BRAM)

testProgram :: Program
testProgram =
    (PushR 2):>
    (Push 0.9765625):>
    (Calc Mult):>
    (Save 2):>

    (PushR 2):>
    (Push 0.0390625):>
    (Calc Mult):>
    (PushR 1):>
    (Calc Add):>
    (Save 1):>

    (PushR 1):>
    (Push 0.0390625):>
    (Calc Mult):>
    (PushR 0):>
    (Calc Add):>
    (Save 0):>

    Nil
    ++
    repeat Nop

fetcher :: State -> Proc.Instr -> (State, Output)
fetcher (pc, stallt) instr = undefined -- add your definition


{-# NOINLINE instrBRAM #-}
instrBRAM :: HiddenClockResetEnable dom
  => Signal dom Address -> Signal dom (Maybe (Address, Proc.Instr)) -> Signal dom Proc.Instr
instrBRAM = blockRam $ testProgram


{-# NOINLINE fetchBlock #-}
fetchBlock :: HiddenClockResetEnable dom
  => Signal dom Proc.Instr -> Signal dom Output
fetchBlock = undefined -- add your definition


{-# NOINLINE system #-}
system :: HiddenClockResetEnable dom => Signal dom Proc.Instr
system = undefined -- add your definition


testSystem = mapM_ print $ sampleN @System 40 system

