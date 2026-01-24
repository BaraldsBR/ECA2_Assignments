{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}
module FiltersXX where

import Clash.Prelude
import FilterCoefAndInput
import qualified Data.List as L

type Clk = Clock System
type Rst = Reset System
type Sig = Signal System

hs :: Vec 6 (Signed 8)
hs = (3:>5:>7:>11:>13:>17:>Nil)

fir3t_6 :: Vec 6 (Signed 8) -> Signed 8 -> (Vec 6 (Signed 8), Signed 8)
fir3t_6 state x = (newState, output) 
  where
    output = head state

    prod :: Vec 3 (Signed 8)
    prod = zipWith (*) (take d3 hs) (repeat x)

    newState = (zipWith (+) (tail state) (take d5 (prod ++ (reverse prod)))) ++ ((head prod):> Nil)

mfir3t_6 :: HiddenClockResetEnable dom => Signal dom (Signed 8) -> Signal dom (Signed 8)
mfir3t_6 = mealy fir3t_6 (repeat 0)

simMfir3t_6 :: [Signed 8] -> [Signed 8]
simMfir3t_6 inputList = simulateN @System 20 mfir3t_6 inputList

simulate20 :: [Signed 8]
simulate20 = simMfir3t_6 [1..20]

-- Assignment 7
{-# ANN synth_mfir3t_6
  (Synthesize
    { t_name   = "mfir3t_6"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_mfir3t_6 :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
synth_mfir3t_6 clk rst = withClockResetEnable clk rst enableGen mfir3t_6
