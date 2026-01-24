{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}
module FiltersXX where

import Clash.Prelude
import FilterCoefAndInput
import qualified Data.List as L

type Clk = Clock System
type Rst = Reset System
type Sig = Signal System

fir1_100 :: HiddenClockResetEnable dom => Signal dom (SFixed 5 13) -> Signal dom (SFixed 5 13)
fir1_100 = mealy step (repeat 0)
  where
    step :: Vec 100 (SFixed 5 13) -> SFixed 5 13 -> (Vec 100 (SFixed 5 13), SFixed 5 13)
    step state input = (newState, output)
      where
        newState = input :> init state
        output = fold (+) (zipWith (*) filterCoef newState)

-----------------------------------------------------------
-- topEntity's
-----------------------------------------------------------
-- Assignment 2
{-# ANN synth_fir1_100
  (Synthesize
    { t_name   = "fir1_100"
    , t_inputs = [PortName "xs"]
    , t_output = PortName "o"
    }) #-}
synth_fir1_100 :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
synth_fir1_100 clk rst = withClockResetEnable clk rst enableGen fir1_100
