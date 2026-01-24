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
module FiltersXX where

import Clash.Prelude
import FilterCoefAndInput
import qualified Data.List as L

type Clk = Clock System
type Rst = Reset System
type Sig = Signal System

hs :: Vec 6 (Signed 8)
hs = (3:>5:>7:>11:>13:>17:>Nil)

fir1_6 :: HiddenClockResetEnable dom => Signal dom (Signed 8) -> Signal dom (Signed 8)
fir1_6 input = out
  where
    reg1 = register 0 input
    reg2 = register 0 reg1
    reg3 = register 0 reg2
    reg4 = register 0 reg3
    reg5 = register 0 reg4
    reg6 = register 0 reg5
    
    delayedVec = bundle (reg6 :> reg5 :> reg4 :> reg3 :> reg2 :> reg1 :> Nil)
    
    out = fmap (fold (+) . zipWith (*) hs) delayedVec

-----------------------------------------------------------
-- topEntity's
-----------------------------------------------------------
{-# ANN synth_fir1_6
  (Synthesize
    { t_name   = "fir1_6"
    , t_inputs = [PortName "xs"]
    , t_output = PortName "o"
    }) #-}
synth_fir1_6 :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
synth_fir1_6 clk rst = withClockResetEnable clk rst enableGen fir1_6