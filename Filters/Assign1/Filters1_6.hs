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

fir1_6 :: HiddenClockResetEnable dom => Signal dom (Vec 6 (Signed 8)) -> Signal dom (Signed 8)
fir1_6 input = output
  where
    state = register hs (pure hs)
    output = fold (+) <$> (zipWith (*) <$> input <*> state)

-----------------------------------------------------------
-- topEntity's
-----------------------------------------------------------
{-# ANN synth_fir1_6
  (Synthesize
    { t_name   = "fir1_6"
    , t_inputs = [PortName "xs"]
    , t_output = PortName "o"
    }) #-}
synth_fir1_6 :: Clk -> Rst -> Sig (Vec 6 (Signed 8)) -> Sig (Signed 8)
synth_fir1_6 clk rst = withClockResetEnable clk rst enableGen fir1_6