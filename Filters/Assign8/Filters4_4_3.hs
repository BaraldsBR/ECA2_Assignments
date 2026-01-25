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

iir1 :: (
        Vec 3 (SFixed 5 13),
        Vec 4 (SFixed 5 13),
        Vec 3 (SFixed 5 13)
    )-> SFixed 5 13 
    -> (
        (Vec 3 (SFixed 5 13),
        Vec 4 (SFixed 5 13),
        Vec 3 (SFixed 5 13)
    ), SFixed 5 13)
iir1 (as, bs, regs) x = ((as, bs, newRegs), output) 
  where
    output = (head regs) + (head bProds)

    bProds = zipWith (*) bs (repeat x)
    aProds = zipWith (*) as (repeat output)
    abSums = zipWith (+) (tail bProds) aProds
    
    newRegs = (zipWith
            (+) (init abSums) (tail regs))
            ++ ((last abSums):>Nil)

miir1 :: HiddenClockResetEnable dom 
      => Signal dom (SFixed 5 13) 
      -> Signal dom (SFixed 5 13)
miir1 = mealy iir1 (baseAs, baseBs, repeat 0)

simMiir1 :: [SFixed 5 13]
simMiir1 = simulateN @System 40 miir1 (1:L.replicate 49 0)

-- Assignment 8
{-# ANN synth_miir1
  (Synthesize
    { t_name   = "miir1"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_miir1 :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
synth_miir1 clk rst = withClockResetEnable clk rst enableGen miir1
