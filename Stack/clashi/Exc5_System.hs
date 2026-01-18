{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore #-}
{-
Student information:
  Student 1
    lastname: Souza
    student number: s3751163
  Student 2
    lastname: Daskalov
    student number: s2150883
-}
module Exc5_System where

import Clash.Prelude

import qualified Exc2_2_Stack as Stack
import qualified Exc3_2_Processor as Proc
import qualified Exc4_Fetch as Fetch


system :: HiddenClockResetEnable dom => Signal dom (Maybe Proc.Value)
system = output
  where
    output = Proc.system procInstr
    procInstr = Fetch.system

testSystem = mapM_ print $ sampleN @System 32 system
