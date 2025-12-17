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
module Exc1 where

import Clash.Prelude
import Clash.Signal

import qualified Data.List as L

xor' :: Unsigned 1 -> Unsigned 1 -> Unsigned 1
xor' 0 0 = 0
xor' 0 1 = 1
xor' 1 0 = 1
xor' 1 1 = 0

not' :: Unsigned 1 -> Unsigned 1
not' 0 = 1
not' 1 = 0

exampleSystem :: HiddenClockResetEnable dom =>
    Signal dom (Unsigned 1) -> Signal dom (Unsigned 1)
exampleSystem x = output
    where
        output = xor' <$> notout <*> x
        notout = not' <$> regout
        regout = register 0 output

system :: HiddenClockResetEnable dom => Signal dom (Unsigned 1) -> Signal dom (Unsigned 1)
system x = undefined -- add your definition


testSystem1 = simulateN @System len system inp
  where
    inp = [0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1]
    len = L.length inp

testSystem2 = simulateN @System len system inp
  where
    inp = [1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0]
    len = L.length inp