{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}
module FiltersXX where

import Clash.Prelude
import FilterCoefAndInput
import qualified Data.List as L

type Clk = Clock System
type Rst = Reset System
type Sig = Signal System

baseAs :: Vec 3 (SFixed 5 13)
baseAs = (0.9853304:>(-0.5929545):>0.1089457:>Nil)

baseBs :: Vec 4 (SFixed 5 13)
baseBs = (0.0623348:>0.1870044:>0.1870044:>0.0623348:>Nil)

iir2 :: (
        Vec 3 (SFixed 5 13),
        Vec 4 (SFixed 5 13),
        Vec 3 (SFixed 5 13)
    )-> SFixed 5 13 
    -> (
        (Vec 3 (SFixed 5 13),
        Vec 4 (SFixed 5 13),
        Vec 3 (SFixed 5 13)
    ), SFixed 5 13)
iir2 (as, bs, regs) x = ((as, bs, newRegs), output) 
  where
    output = fold (+) bProds

    bProds = zipWith (*) bs ((inputSum:>Nil) ++ regs)
    aProds = zipWith (*) as regs
    aSum = fold (+) aProds
    inputSum = aSum + x

    newRegs = (inputSum:>Nil) ++ (init regs)

miir2 :: HiddenClockResetEnable dom
  => Signal dom (SFixed 5 13)
  -> Signal dom (SFixed 5 13)
miir2 = mealy iir2 (baseAs, baseBs, repeat 0)

simMiir2 :: [SFixed 5 13]
simMiir2 = simulateN @System 40 miir2 (1:L.replicate 49 0)

-- Assignment 9
{-# ANN synth_miir2
  (Synthesize
    { t_name   = "miir2"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_miir2 :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
synth_miir2 clk rst = withClockResetEnable clk rst enableGen miir2
