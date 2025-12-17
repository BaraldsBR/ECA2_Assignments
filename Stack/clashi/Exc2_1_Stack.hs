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
module Exc2_1_Stack where

import Clash.Prelude
import Clash.Signal

import qualified Data.List as L

type Address = Unsigned 3
type Value = SFixed 8 8

data Instr = Push Value | Nop
  deriving (Show, Generic, NFDataX)

type State = (Address) -- the stack pointer

type Output = (Address, Maybe (Address, Value))

stackController :: State -> Instr  -> (State, Output)
stackController sp instr = undefined -- add your definition


stackBlock :: HiddenClockResetEnable dom
  => Signal dom Instr -> Signal dom Output
stackBlock = undefined -- add your definition


blockRAMblock :: HiddenClockResetEnable dom
  => Signal dom Address -> Signal dom (Maybe (Address, Value)) -> Signal dom Value
blockRAMblock = blockRam $ 0:>0:>0:>0:>0:>0:>0:>0:>Nil


system :: HiddenClockResetEnable dom
  => Signal dom Instr -> Signal dom Value
system instr = undefined -- add your definition


testStackBlock = mapM_ print $ simulateN @System len stackBlock inp
  where
    inp = [Nop, Nop, Push 10, Push 11, Push 12, Nop, Nop, Nop, Nop, Push 1, Nop]
    len = L.length inp

testSystem = mapM_ print $ simulateN @System len system inp
  where
    inp = [Nop, Nop, Push 1, Push 2, Push 3, Push 4, Push 5, Push 6, Push 7, Push 8, Push 9, Push 10]
    len = L.length inp


testBlk = mapM_ print $ simulateN @System (L.length testinput) testblk testinput
  where
    testinput :: [(Address, Maybe (Address, Value))]
    testinput =
        [ (0, Nothing)
        , (0, Nothing)
        , (0, Just (0, 10))
        , (0, Just (1, 20))
        , (1, Nothing)
        , (0, Nothing)
        , (2, Nothing)
        , (2, Nothing)]

    testblk input = blockRAMblock read write
      where (read, write) = unbundle input

