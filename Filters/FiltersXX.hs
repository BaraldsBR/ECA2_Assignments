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

-- Student information:
--  Student 1
--    lastname:
--    student number:
--  Student 2
--    lastname:
--    student number:

-- NOTE: synthesis annotated functions are at the bottom
-- comment and uncomment the functions for each assignment
-- it should not be necessary, but you may change the
-- definition of the synthesis annotated functions.

hs = (3:>5:>7:>11:>13:>17:>Nil)

fir1_6 = undefined

-----------------------------------------------------------
-- Assignment 2
-- FIR1 N = 100
-----------------------------------------------------------

fir1_100 = undefined

-----------------------------------------------------------
-- Assignment 3
-- FIR2 N = 6
-----------------------------------------------------------

fir2 = undefined

mfir2_6 :: HiddenClockResetEnable dom => Signal dom (Signed 8) -> Signal dom (Signed 8)
mfir2_6 = undefined

simMfir2_6 = undefined

-----------------------------------------------------------
-- Assignment 4
-- FIR2 N = 100
-----------------------------------------------------------

mfir2_100 :: HiddenClockResetEnable dom => Signal dom (SFixed 5 13) -> Signal dom (SFixed 5 13)
mfir2_100 = undefined

-----------------------------------------------------------
-- Assignment 5
-- FIR3 N = 6
-----------------------------------------------------------

fir3 = undefined

mfir3_6 :: HiddenClockResetEnable dom => Signal dom (Signed 8) -> Signal dom (Signed 8)
mfir3_6 = undefined

simMfir3_6 = undefined

-----------------------------------------------------------
-- Assignment 6
-- FIR3 N = 100
-----------------------------------------------------------

mfir3_100 :: HiddenClockResetEnable dom => Signal dom (SFixed 5 13) -> Signal dom (SFixed 5 13)
mfir3_100 = undefined

-----------------------------------------------------------
-- Assignment 7
-- FIR3t N = 6
-----------------------------------------------------------

fir3t = undefined

mfir3t_6 :: HiddenClockResetEnable dom => Signal dom (Signed 8) -> Signal dom (Signed 8)
mfir3t_6 = undefined

simMfir3t_6 = undefined

-----------------------------------------------------------
-- Assignment 8
-- IIR
-----------------------------------------------------------

bs = (0.0623348 :> 0.1870044 :> 0.1870044 :> 0.0623348 :> Nil)
as = (0.9853304 :> -0.5929545 :> 0.1089457 :> Nil)

iir1 = undefined

miir1 :: HiddenClockResetEnable dom => Signal dom (SFixed 5 13) -> Signal dom (SFixed 5 13)
miir1 = undefined

simMiir1 = undefined

-----------------------------------------------------------
-- Assignment 9
-- IIR
-----------------------------------------------------------

iir2 = undefined

miir2 :: HiddenClockResetEnable dom => Signal dom (SFixed 5 13) -> Signal dom (SFixed 5 13)
miir2 = undefined

simMiir2 = undefined

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
-- synth_fir1_6 ::
synth_fir1_6 = fir1_6
-}
{-
-- Assignment 2
{-# ANN synth_fir1_100
  (Synthesize
    { t_name   = "fir1_100"
    , t_inputs = [PortName "xs"]
    , t_output = PortName "o"
    }) #-}
--synth_fir1_100 ::
synth_fir1_100 = fir1_100
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

