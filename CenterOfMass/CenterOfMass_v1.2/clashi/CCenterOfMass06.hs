{-# LANGUAGE NoMonomorphismRestriction #-}
module CCenterOfMassXX where
import Clash.Prelude
import Axi
import Common
import ComAxisTb
import Data.Maybe
import qualified Data.List as L
-- import ImageData
-- import Debug.Trace

-- Student information:
--  Student 1
--    lastname:
--    student number:
--  Student 2
--    lastname:
--    student number:


-----------------------------------------------------------------------------------------
-- Assignment 4, Changing a pixel in a picture
-----------------------------------------------------------------------------------------

changePixelInImage :: (Enum a, KnownNat n, KnownNat m)
  => Vec n (Vec m Pixel)
  -> a 
  -> a
  -> Pixel
  -> Vec n (Vec m Pixel)
changePixelInImage image y x p = replace y (replace x p (image !! y)) image

thresholdIm :: (KnownNat n, KnownNat m)
  => Integer
  -> Vec n (Vec m Pixel)
  -> Vec n (Vec m Pixel)
thresholdIm t grayscaleImage = bwImage where
  applyThreshold :: Pixel -> Pixel
  applyThreshold p = if (fromIntegral p) < t then 0 else 1
  bwImage = map (map applyThreshold) grayscaleImage

comRows :: (KnownNat n, KnownNat m)
  => Vec n (Vec m Pixel)
  -> Int
comRows image = rowIndex where
  rowIndex = div totalProductSum totalSum
  
  rowSums = map sumFromInt image where sumFromInt a = sum (map fromIntegral a)
  totalSum = sum rowSums

  productRowSums = zipWith (*) rowSums (iterateI (+1) 0)
  totalProductSum = sum productRowSums

com :: (KnownNat n, KnownNat m)
  => Vec n (Vec m Pixel) -> (Int, Int)
com image = (rowIndex, colIndex) where
  rowIndex = comRows image
  colIndex = comRows $ transpose image

imageWithCom :: (KnownNat n, KnownNat m)
  => Pixel
  -> Vec n (Vec m Pixel)
  -> Vec n (Vec m Pixel)
imageWithCom color image = modifiedImage where
  (rowIndex, colIndex) = com image
  modifiedImage = changePixelInImage image rowIndex colIndex color

-----------------------------------------------------------------------------------------
-- Assignment 5, Center of mass of parts of the image, with and without borders
-----------------------------------------------------------------------------------------

comParts :: (KnownNat n, KnownNat m)
  => Vec (n * 8) (Vec (m * 8) Pixel)
  -> Vec (n * 8) (Vec (m * 8) Pixel)
comParts image = withSNat (unblocks2D) modifiedBlocks where
  
  isZeroes :: Vec 8 (Vec 8 Pixel) -> Bool
  isZeroes block = block == (repeat $ repeat (0 :: Pixel))

  blocks = blocks2D d8 image
  modifiedBlocks = map alterBlock blocks where
    alterBlock block = if (isZeroes block)
      then changePixelInImage block 3 3 2
      else imageWithCom 2 block
            
comPartsWB :: (KnownNat n, KnownNat m)
  => Vec (n * 8) (Vec (m * 8) Pixel)
  -> Vec (n * 10) (Vec (m * 10) Pixel)
comPartsWB image = withSNat (unblocks2D) modifiedBlocksWB where

  isZeroes :: Vec 8 (Vec 8 Pixel) -> Bool
  isZeroes block = block == (repeat $ repeat (0 :: Pixel))

  blocks = blocks2D d8 image
  modifiedBlocks = map alterBlock blocks where
    alterBlock block = if (isZeroes block)
      then changePixelInImage block 3 3 2
      else imageWithCom 2 block
  modifiedBlocksWB = addBorders 2 modifiedBlocks

-----------------------------------------------------------------------------------------
-- Assignment 6, Axi streaming serial
-----------------------------------------------------------------------------------------

axisComSer state (s_axi, m_axis_tready) = (state', (m_axi, s_axis_tready))
  where
    state' = undefined
    m_axi = undefined
    s_axis_tready = undefined


mAxisComSer :: (HiddenClockResetEnable dom)
  => Signal dom (Maybe (Axi4Stream (Signed 32) (BitVector 4)), Bool)
  -> Signal dom (Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool)
mAxisComSer mInp = undefined

-----------------------------------------------------------------------------------------
-- You can use the simulation function spsAxisComSerTb to print out all the iner stages of the states
-- spsAxisComSer :: [(Maybe (Axi4Stream (Signed 32) (BitVector 4)), Bool)] -> String
-- spsAxisComSer inp = simPrintState axisComSer initState inp -- Same as mealy
--   where
--     initState = undefined

-- spsAxisComSerTb :: IO ()
-- spsAxisComSerTb = putStrLn $ spsAxisComSer mAxisComSerInp
-----------------------------------------------------------------------------------------

simMAxisComSerTb :: [(Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool)]
simMAxisComSerTb = simulateN @System (L.length mAxisComSerInp) mAxisComSer mAxisComSerInp
simMAxisComSerTbPrint = mapM_ print $ L.zip [1..] simMAxisComSerTb
simMAxisComSerTbReport = L.filter (\(_,(m,_))-> isJust m) (L.zip [1..] simMAxisComSerTb)

-----------------------------------------------------------------------------------------
-- Assignment 7, Synthesize serial Axi
-----------------------------------------------------------------------------------------

{-# ANN synthAxisComSer
  (Synthesize
    { t_name   = "synthAxisComSer"
    , t_inputs =
      [ PortName "aclk"
      , PortName "nrst"
      , PortProduct ""
        [ PortProduct "s_axis"
          [ PortName "tvalid"
          , PortProduct ""
            [ PortName "tdata"
            , PortName "tlast"
            , PortName "tkeep" ]]
        , PortName "m_axis_tready" ]]
    , t_output = PortProduct ""
        [ PortProduct "m_axis"
          [ PortName "tvalid"
          , PortProduct ""
            [ PortName "tdata"
            , PortName "tlast"
            , PortName "tkeep" ]]
        , PortName "s_axis_tready" ]
    }) #-}
synthAxisComSer ::
     Clock System -- aclk
  -> Reset System -- nrst
  -> Signal System (Maybe (Axi4Stream (Signed 32) (BitVector 4)), Bool)
  -> Signal System (Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool)
synthAxisComSer clk rst inp = exposeClockResetEnable mAxisComSer clk rst enableGen inp

-----------------------------------------------------------------------------------------
-- Assignment 8, Axi streaming parallel
-----------------------------------------------------------------------------------------

axisComPar state (s_axi, m_axis_tready) = (state', (m_axi, s_axis_tready))
  where
    state' = undefined
    m_axi = undefined
    s_axis_tready = undefined

mAxisComPar :: (HiddenClockResetEnable dom)
  => Signal dom (Maybe (Axi4Stream (Vec 16 (Unsigned 8)) (BitVector 16)), Bool)
  -> Signal dom (Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool)
mAxisComPar mInp = undefined

-----------------------------------------------------------------------------------------
-- You can use the simulation function spsAxisComParTb to print out all the iner stages of the states
-- spsAxisComPar :: [(Maybe (Axi4Stream (Vec 16 (Unsigned 8)) (BitVector 16)), Bool)] -> String
-- spsAxisComPar inp = simPrintState axisComPar initState inp -- Same as mealy
--   where
--     initState = undefined

-- spsAxisComParTb :: IO ()
-- spsAxisComParTb = putStrLn $ spsAxisComPar mAxisComParInp
-----------------------------------------------------------------------------------------

simMAxisComParTb :: [(Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool)]
simMAxisComParTb = simulateN @System (L.length mAxisComParInp) mAxisComPar mAxisComParInp
simMAxisComParTbPrint = mapM_ print $ L.zip [1..] simMAxisComParTb
simMAxisComParTbReport = L.filter (\(_,(m,_))-> isJust m) (L.zip [1..] simMAxisComParTb)

-----------------------------------------------------------------------------------------
-- Assignment 9, Synthesize parallel Axi
-----------------------------------------------------------------------------------------

{-# ANN synthAxisComPar
  (Synthesize
    { t_name   = "synthAxisComPar"
    , t_inputs =
      [ PortName "aclk"
      , PortName "nrst"
      , PortProduct ""
        [ PortProduct "s_axis"
          [ PortName "tvalid"
          , PortProduct ""
            [ PortName "tdata"
            , PortName "tlast"
            , PortName "tkeep" ]]
        , PortName "m_axis_tready" ]]
    , t_output = PortProduct ""
        [ PortProduct "m_axis"
          [ PortName "tvalid"
          , PortProduct ""
            [ PortName "tdata"
            , PortName "tlast"
            , PortName "tkeep" ]]
        , PortName "s_axis_tready" ]
    }) #-}
synthAxisComPar ::
     Clock System -- aclk
  -> Reset System -- nrst
  -> Signal System (Maybe (Axi4Stream (Vec 16 (Unsigned 8)) (BitVector 16)), Bool)
  -> Signal System (Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool)
synthAxisComPar clk rst inp = exposeClockResetEnable mAxisComPar clk rst enableGen inp


