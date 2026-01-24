{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}
module FiltersXX where

import Clash.Prelude
import FilterCoefAndInput
import qualified Data.List as L

type Clk = Clock System
type Rst = Reset System
type Sig = Signal System

hs = (3:>5:>7:>Nil) -- Symmetric filter since original could not have worked.

{-
  Given the original function
    h_0 * x_0 + h_1 * x_1 + h_2 * x_2 + h_2 * x_3 + h_1 * x_4 + h_0 * x_5
  
  We can simplify to:
    h_0 * (x_0 + x_5) + h_1 * (x_1 + x_4) + h_2 * (x_2 + x_3)
-}

fir3 :: Vec 6 (Signed 8) -> Signed 8
fir3 state = out
  where
    (x0:>x1:>x2:>x3:>x4:>x5:>Nil) = state
    
    sum_h0 = x0 + x5
    sum_h1 = x1 + x4  
    sum_h2 = x2 + x3
    
    prod_h0 = sum_h0 * (hs !! 0)
    prod_h1 = sum_h1 * (hs !! 1)
    prod_h2 = sum_h2 * (hs !! 2)
    
    out = prod_h0 + prod_h1 + prod_h2

mfir3_6 :: HiddenClockResetEnable dom => Signal dom (Signed 8) -> Signal dom (Signed 8)
mfir3_6 = mealy step (repeat 0)
  where
    step :: Vec 6 (Signed 8) -> Signed 8 -> (Vec 6 (Signed 8), Signed 8)
    step state input = (newState, output)
      where
        newState = input :> init state
        output = fir3 newState

simMfir3_6 :: [Signed 8] -> [Signed 8]
simMfir3_6 inputList = simulateN @System 20 mfir3_6 inputList

simulate20 :: [Signed 8]
simulate20 = simMfir3_6 [1..20]


-- Assignment 5
{-# ANN synth_mfir3_6
  (Synthesize
    { t_name   = "mfir3_6"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_mfir3_6 :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
synth_mfir3_6 clk rst = withClockResetEnable clk rst enableGen mfir3_6