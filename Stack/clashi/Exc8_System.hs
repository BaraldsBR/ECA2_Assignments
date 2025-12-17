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
module Exc8_System where

import Clash.Prelude

import qualified Exc2_2_Stack as Stack
import qualified Exc6_Processor as Proc
import qualified Exc7_Fetch as Fetch
import qualified Data.List as L


system :: HiddenClockResetEnable dom => Signal dom (Proc.Value)
system = undefined -- paste your definition of the composed system of excercise 5


system' :: HiddenClockResetEnable dom => Vec 4 (Proc.RegisterFile) -> Signal dom (Vec 4 (Proc.Value))
system' regs = undefined -- add your definition here




testSystem = mapM_ print $ sampleN @System 40 system

testSystem' regs = mapM_ print $ sampleN @System 100 (system' regs)



topEntity :: Clock System -> Reset System -> Signal System (Proc.Value)
topEntity clk rst = exposeClockResetEnable system clk rst enableGen


-- helper functions
wf regs = writeFile "output.csv" (unlines $ L.map possToString inbounds)
    where
        possToString poss = L.init $ L.foldl (\str p -> str L.++ (show p) L.++ ",") "" (toList $ poss)
        inbounds = takeWhile areInBounds positions
        areInBounds poss = and $ map (\p -> p < 127 && p > -127) poss
        positions = everyNth 30 $ run regs


run regs = sample @System (system' regs)


everyNth n (e:list) = e : (everyNth n rest)
    where
        rest = L.drop (n-1) list