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
fetcher (pc, stallt) instr 
    | stallt > 0                = ((pc, stallt - 1), (Proc.Nop, pc))
    | otherwise                 = ((pc + 1, delay), (instr, pc + 1)) 
        where 
            delay = case instr of
                Proc.Calc _ -> 2
                _           -> 0
        


instrBRAM :: HiddenClockResetEnable dom =>
    Signal dom Address -> Signal dom (Maybe (Address, Proc.Instr)) -> Signal dom Proc.Instr
instrBRAM = blockRam $ testProgram


fetchBlock :: HiddenClockResetEnable dom =>
    Signal dom Proc.Instr -> Signal dom Output
fetchBlock = mealy fetcher (0, 1)


system :: HiddenClockResetEnable dom => Signal dom Proc.Instr
system = procInstr
    where
        (procInstr, addr) = unbundle $ fetchBlock fetchInstr
        fetchInstr = instrBRAM addr (pure Nothing)


testSystem = mapM_ print $ sampleN @System 32 system
