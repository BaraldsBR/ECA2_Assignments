module HCenterOfMassXX where

import Image
import Data.List

-- Student information:
--  Student 1
--    lastname:
--    student number:
--  Student 2
--    lastname:
--    student number:

-----------------------------------------------------------------------------------------
-- Assignment 1, From grayscale to black and white
-----------------------------------------------------------------------------------------

threshold = 128 -- not sure if we should import this from Common.hs, since that depends focuses on hardware and this is purely functional

thresholdIm :: [[Pixel]] -> [[Pixel]]
thresholdIm grayscaleImage = bwImage where
    applyThreshold p = if p < threshold then 0 else 1
    bwImage = map (map applyThreshold) grayscaleImage

lightHouseBW :: FilePath
lightHouseBW = "../images/lightHouseBW.pgm"

-----------------------------------------------------------------------------------------
-- Assignment 2, Center of mass of rows and picture
-----------------------------------------------------------------------------------------

comRows :: [[Pixel]] -> Int
comRows image = rowIndex where
    rowIndex = div totalProductSum totalSum
    
    rowSums = map sum image
    totalSum = sum rowSums

    productRowSums = zipWith (*) rowSums [0..]
    totalProductSum = sum productRowSums

com :: [[Pixel]] -> (Int, Int)
com image = (rowIndex, colIndex) where
    rowIndex = comRows image
    colIndex = comRows $ transpose image

imageWithCom :: Int -> [[Pixel]] -> [[Pixel]]
imageWithCom color image = modifiedImage where
    (rowIndex, colIndex) = com image
    modifiedImage = changePixelInImage image rowIndex colIndex color

-----------------------------------------------------------------------------------------
-- Assignment 3 Center of mass of parts of the image, with and without borders
-----------------------------------------------------------------------------------------

comParts :: [[Pixel]] -> [[Pixel]]
comParts image = unblocks2D (length image) modifiedBlocks where
    isZeroes :: [[Pixel]] -> Bool
    isZeroes block = block == (take 8 $ repeat $ take 8 $ repeat 0)

    blocks = blocks2D 8 image
    modifiedBlocks = map alterBlock blocks where
        alterBlock block = if (isZeroes block)
            then changePixelInImage block 3 3 2
            else imageWithCom 2 block
            
comPartsWB :: [[Pixel]] -> [[Pixel]]
comPartsWB image = unblocks2D (div ((length image) * 5) 4) modifiedBlocksWB where
    isZeroes :: [[Pixel]] -> Bool
    isZeroes block = block == (take 8 $ repeat $ take 8 $ repeat 0)

    blocks = blocks2D 8 image
    modifiedBlocks = map alterBlock blocks where
        alterBlock block = if (isZeroes block)
            then changePixelInImage block 3 3 2
            else imageWithCom 2 block
    modifiedBlocksWB = addBorders 2 modifiedBlocks