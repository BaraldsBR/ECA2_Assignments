{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}
{-
Student information:
  Student 1
    lastname: Baraldi Souza
    student number: s3751163
  Student 2
    lastname: Daskalov
    student number: s2150883
-}
module FiltersXX where

import Clash.Prelude
import FilterCoefAndInput
import qualified Data.List as L

type Clk = Clock System
type Rst = Reset System
type Sig = Signal System

-- NOTE: synthesis annotated functions are at the bottom
-- comment and uncomment the functions for each assignment
-- it should not be necessary, but you may change the
-- definition of the synthesis annotated functions.

hs :: Vec 6 (Signed 8)
hs = (3:>5:>7:>11:>13:>17:>Nil)

fir1_6 :: HiddenClockResetEnable dom => Signal dom (Vec 6 (Signed 8)) -> Signal dom (Signed 8)
fir1_6 input = output
  where
    state = register hs (pure hs)
    output = fold (+) <$> (zipWith (*) <$> input <*> state)

-----------------------------------------------------------
-- Assignment 2
-- FIR1 N = 100
-----------------------------------------------------------

fir1_100 :: HiddenClockResetEnable dom => Signal dom (Vec 100 (SFixed 5 13)) -> Signal dom (SFixed 5 13)
fir1_100 input = output
  where
    state = register filterCoef (pure filterCoef)
    output = fold (+) <$> (zipWith (*) <$> input <*> state)

-----------------------------------------------------------
-- Assignment 3
-- FIR2 N = 6
-----------------------------------------------------------

fir2 :: Vec 6 (Signed 8) -> Signed 8
fir2 state = fold (+) (zipWith (*) hs state)

mfir2_6 :: HiddenClockResetEnable dom => Signal dom (Signed 8) -> Signal dom (Signed 8)
mfir2_6 = mealy step (repeat 0)
  where
    step :: Vec 6 (Signed 8) -> Signed 8 -> (Vec 6 (Signed 8), Signed 8)
    step state input = (newState, output)
      where
        newState = input :> init state 
        output = fir2 state

simMfir2_6 :: [Signed 8] -> [Signed 8]
simMfir2_6 inputList = simulateN @System 20 mfir2_6 inputList

simulate20 :: [Signed 8]
simulate20 = simMfir2_6 [1..20]

-----------------------------------------------------------
-- Assignment 4
-- FIR2 N = 100
-----------------------------------------------------------

fir100 :: Vec 100 (SFixed 5 13) -> SFixed 5 13
fir100 state = fold (+) (zipWith (*) filterCoef state)

mfir2_100 :: HiddenClockResetEnable dom => Signal dom (SFixed 5 13) -> Signal dom (SFixed 5 13)
mfir2_100 = mealy step (repeat 0)
  where
    step :: Vec 100 (SFixed 5 13) -> SFixed 5 13 -> (Vec 100 (SFixed 5 13), SFixed 5 13)
    step state input = (newState, output)
      where
        newState = input :> init state
        output = fir100 state

simMfir2_100 :: [SFixed 5 13] -> [SFixed 5 13]
simMfir2_100 inputList = simulate @System mfir2_100 inputList

simulateIn :: [SFixed 5 13]
simulateIn = simMfir2_100 inputSignal

saveSimulationToCSV :: IO ()
saveSimulationToCSV = writeFile "output.csv" (unlines $ L.map show simulateIn)

-----------------------------------------------------------
-- Assignment 5
-- FIR3 N = 6
-----------------------------------------------------------

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
    
    prod_h0 = (hs !! 0) * (x0 + x5)
    prod_h1 = (hs !! 1) * (x1 + x4)
    prod_h2 = (hs !! 2) * (x2 + x3)
    
    out = prod_h0 + prod_h1 + prod_h2

mfir3_6 :: HiddenClockResetEnable dom => Signal dom (Signed 8) -> Signal dom (Signed 8)
mfir3_6 = mealy step (repeat 0)
  where
    step :: Vec 6 (Signed 8) -> Signed 8 -> (Vec 6 (Signed 8), Signed 8)
    step state input = (newState, output)
      where
        newState = input :> init state
        output = fir3 state

simMfir3_6 :: [Signed 8] -> [Signed 8]
simMfir3_6 inputList = simulateN @System 20 mfir3_6 inputList

simulate20 :: [Signed 8]
simulate20 = simMfir3_6 [1..20]

-----------------------------------------------------------
-- Assignment 6
-- FIR3 N = 100
-----------------------------------------------------------

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

saveSimulationToCSV :: IO ()
saveSimulationToCSV = writeFile "output.csv" (unlines $ L.map show simulateIn)

-----------------------------------------------------------
-- Assignment 7
-- FIR3t N = 6
-----------------------------------------------------------

fir3t_6 :: Vec 6 (Signed 8) -> Signed 8 -> (Vec 6 (Signed 8), Signed 8)
fir3t_6 state x = (newState, output) 
  where
    output = head state

    prod :: Vec 3 (Signed 8)
    prod = zipWith (*) (take d3 hs) (repeat x)

    newState = (zipWith 
             (+) (tail state) (take d5 (prod ++ (reverse prod)))) 
             ++ ((head prod):> Nil)

mfir3t_6 :: HiddenClockResetEnable dom => Signal dom (Signed 8) -> Signal dom (Signed 8)
mfir3t_6 = mealy fir3t_6 (repeat 0)

simMfir3t_6 :: [Signed 8] -> [Signed 8]
simMfir3t_6 inputList = simulateN @System 20 mfir3t_6 inputList

simulate20 :: [Signed 8]
simulate20 = simMfir3t_6 [1..20]

-----------------------------------------------------------
-- Assignment 8
-- IIR
-----------------------------------------------------------

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

-----------------------------------------------------------
-- Assignment 9
-- IIR
-----------------------------------------------------------

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

-----------------------------------------------------------
-- topEntity's
-----------------------------------------------------------
{-
-- Assignment 1
{-# ANN synth_fir1_6
  (Synthesize
    { t_name   = "fir1_6"
    , t_inputs = [PortName "xs"]
    , t_output = PortName "o"
    }) #-}
synth_fir1_6 :: Clk -> Rst -> Sig (Vec 6 (Signed 8)) -> Sig (Signed 8)
synth_fir1_6 clk rst = withClockResetEnable clk rst enableGen fir1_6
-}
{-
-- Assignment 2
{-# ANN synth_fir1_100
  (Synthesize
    { t_name   = "fir1_100"
    , t_inputs = [PortName "xs"]
    , t_output = PortName "o"
    }) #-}
synth_fir1_100 :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
synth_fir1_100 clk rst = withClockResetEnable clk rst enableGen fir1_100
-}
{-
-- Assignment 3
{-# ANN synth_mfir2_6
  (Synthesize
    { t_name   = "mfir2_6"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_mfir2_6 :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
synth_mfir2_6 clk rst = withClockResetEnable clk rst enableGen mfir2_6
-}
{-
-- Assignment 4
{-# ANN synth_mfir2_100
  (Synthesize
    { t_name   = "mfir2_100"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_mfir2_100 :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
synth_mfir2_100 clk rst = withClockResetEnable clk rst enableGen mfir2_100
-}
{-
-- Assignment 5
{-# ANN synth_mfir3_6
  (Synthesize
    { t_name   = "mfir3_6"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_mfir3_6 :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
synth_mfir3_6 clk rst = withClockResetEnable clk rst enableGen mfir3_6
-}
{-
-- Assignment 6
{-# ANN synth_mfir3_100
  (Synthesize
    { t_name   = "mfir3_100"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_mfir3_100 :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
synth_mfir3_100 clk rst = withClockResetEnable clk rst enableGen mfir3_100
-}
{-
-- Assignment 7
{-# ANN synth_mfir3t_6
  (Synthesize
    { t_name   = "mfir3t_6"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_mfir3t_6 :: Clk -> Rst -> Sig (Signed 8) -> Sig (Signed 8)
synth_mfir3t_6 clk rst = withClockResetEnable clk rst enableGen mfir3t_6
-}
{-
-- Assignment 8
{-# ANN synth_miir1
  (Synthesize
    { t_name   = "miir1"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_miir1 :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
synth_miir1 clk rst = withClockResetEnable clk rst enableGen miir1
-}
{-
-- Assignment 9
{-# ANN synth_miir2
  (Synthesize
    { t_name   = "miir2"
    , t_inputs = [PortName "clk", PortName "rst", PortName "x"]
    , t_output = PortName "o"
    }) #-}
synth_miir2 :: Clk -> Rst -> Sig (SFixed 5 13) -> Sig (SFixed 5 13)
synth_miir2 clk rst = withClockResetEnable clk rst enableGen miir2
-}

