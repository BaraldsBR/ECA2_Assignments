{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}
module FiltersXX where

import Clash.Prelude
import FilterCoefAndInput
import qualified Data.List as L

type Clk = Clock System
type Rst = Reset System
type Sig = Signal System

fir3 :: Vec 100 (SFixed 5 13) -> SFixed 5 13
fir3 state = out
  where
    (first, second) = splitAt (SNat @50) state
    secondReversed = reverse second
    
    sums :: Vec 50 (SFixed 5 13)
    sums = zipWith (+) first secondReversed

    halfCoeff :: Vec 50 (SFixed 5 13)
    halfCoeff = take (SNat @50) filterCoef
    
    products :: Vec 50 (SFixed 5 13)
    products = zipWith (*) halfCoeff sums
    
    out = fold (+) products

mfir3_100 :: HiddenClockResetEnable dom => Signal dom (SFixed 5 13) -> Signal dom (SFixed 5 13)
mfir3_100 = mealy step (repeat 0)
  where
    step :: Vec 100 (SFixed 5 13) -> SFixed 5 13 -> (Vec 100 (SFixed 5 13), SFixed 5 13)
    step state input = (newState, output)
      where
        newState = input :> init state
        output = fir3 state

simMfir3_100 :: [SFixed 5 13] -> [SFixed 5 13]
simMfir3_100 inputList = simulate @System mfir3_100 inputList

simulateIn :: [SFixed 5 13]
simulateIn = simMfir3_100 inputSignal

-- Assignment 6
{-# ANN synth_mfir3_100
  (Synthesize
    { t_name   = "mfir3_100"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_mfir3_100 :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
synth_mfir3_100 clk rst = withClockResetEnable clk rst enableGen mfir3_100