{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}
module FiltersXX where

import Clash.Prelude
import FilterCoefAndInput
import qualified Data.List as L

type Clk = Clock System
type Rst = Reset System
type Sig = Signal System

fir100 :: Vec 100 (SFixed 5 13) -> SFixed 5 13
fir100 state = fold (+) (zipWith (*) filterCoef state)

mfir2_100 :: HiddenClockResetEnable dom => Signal dom (SFixed 5 13) -> Signal dom (SFixed 5 13)
mfir2_100 = mealy step (repeat 0)
  where
    step :: Vec 100 (SFixed 5 13) -> SFixed 5 13 -> (Vec 100 (SFixed 5 13), SFixed 5 13)
    step state input = (newState, output)
      where
        newState = input :> init state
        output = fir100 newState

simMfir2_100 :: [SFixed 5 13] -> [SFixed 5 13]
simMfir2_100 inputList = simulateN @System 20 mfir2_100 inputList

simulate100 :: [SFixed 5 13]
simulate100 = simMfir2_100 inputSignal

-- Assignment 4
{-# ANN synth_mfir2_100
  (Synthesize
    { t_name   = "mfir2_100"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_mfir2_100 :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
synth_mfir2_100 clk rst = withClockResetEnable clk rst enableGen mfir2_100