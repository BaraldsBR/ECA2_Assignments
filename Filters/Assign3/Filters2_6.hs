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

fir2 :: Vec 6 (Signed 8) -> Signed 8
fir2 state = fold (+) (zipWith (*) hs state)

mfir2_6 :: HiddenClockResetEnable dom => Signal dom (Signed 8) -> Signal dom (Signed 8)
mfir2_6 = mealy step (repeat 0)
  where
    step :: Vec 6 (Signed 8) -> Signed 8 -> (Vec 6 (Signed 8), Signed 8)
    step state input = (newState, output)
      where
        newState = input :> init state 
        output = fir2 newState

simMfir2_6 :: [Signed 8] -> [Signed 8]
simMfir2_6 inputList = simulateN @System 20 mfir2_6 inputList

simulate20 :: [Signed 8]
simulate20 = simMfir2_6 [1..20]

{-# ANN synth_mfir2_6
  (Synthesize
    { t_name   = "mfir2_6"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_mfir2_6 :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
synth_mfir2_6 clk rst = withClockResetEnable clk rst enableGen mfir2_6