module AxisImageGenerator where
import Clash.Prelude
import Common
import Axi
import Data.Char
import ReadPGM
import qualified Data.List as L
import qualified Data.List.Split as S -- cabal install split
import Clash.Sized.Vector as SV

data Mode = SER | PAR deriving (Eq, Show)

-----------------------------------------------------------------------------------------
-- writing image in blocks to file
-----------------------------------------------------------------------------------------
-- TODO: generalize types
makeAxisInpSer :: Integral a => Int -> [[a]] -> [(Maybe (Axi4Stream (Signed 32) (BitVector 4)), Bool)]
makeAxisInpSer ww im = L.zipWith epr [1..] $ L.concat $ L.map L.concat (blocks2DL ww im)
  where
    epr i d = (Just (Axi4Stream (fromIntegral d) ((mod i (ww*ww)) == 0) 0b1111), True)

makeAxisInpPar :: (Integral a, KnownNat n) => SNat n -> Int -> [[a]] -> [(Maybe (Axi4Stream (Vec n (Unsigned 8)) (BitVector n)), Bool)]
makeAxisInpPar pw ww im = L.zipWith epr [1..] $ S.chunksOf spw $ L.concat $ L.map L.concat (blocks2DL ww im)
  where
    spw = snatToNum pw
    epr i d = (Just (Axi4Stream (SV.unsafeFromList $ L.map fromIntegral d) ((mod i (div (ww*ww) spw)) == 0) (v2bv $ replicate pw 1)), True)

makeAxisFileInpSer :: Integral a => Int -> [[a]] -> [(Maybe (Axi4Stream (Signed 32) [Bit]), Bool)]
makeAxisFileInpSer ww im = L.zipWith epr [1..] $ L.concat $ L.map L.concat (blocks2DL ww im)
  where
    epr i d = (Just (Axi4Stream (fromIntegral d) ((mod i (ww*ww)) == 0) (L.replicate 4 1)), True)

makeAxisFileInpPar :: Integral a => Int -> Int -> [[a]] -> [(Maybe (Axi4Stream [Unsigned 8] [Bit]), Bool)]
makeAxisFileInpPar pw ww im = L.zipWith epr [1..] $ S.chunksOf pw $ L.concat $ L.map L.concat (blocks2DL ww im)
  where
    epr i d = (Just (Axi4Stream (L.map fromIntegral d) ((mod i (div (ww*ww) pw)) == 0) (L.replicate pw 1)), True)

makeAxisFile :: Mode -> Int -> Int -> FilePath -> IO ()
makeAxisFile mode ww pw path = do
  im <- readPGM path
  let noExtension = L.drop 1 $ dropWhile (/='.') (L.reverse path)
      name = L.reverse $ takeWhile (\c -> (isLetter c) || (isDigit c)) noExtension
      name' = case mode of
        SER -> name L.++ "Axis"
        PAR -> name L.++ "ParAxis"
      varName = lowercaseFirst name'
      moduleName = uppercaseFirst name'
      fileStart = [ "module " L.++ moduleName L.++ " where"
                  , "import Clash.Prelude"
                  , "import Axi"
                  , case mode of
                      SER -> varName L.++ " :: [(Maybe (Axi4Stream (Signed 32) [Bit]), Bool)]"
                      PAR -> varName L.++ " :: [(Maybe (Axi4Stream [Unsigned 8] [Bit]), Bool)]"
                  , varName L.++ " = ["
                  ]
      fileData = case mode of
                    SER -> (L.map (\(ta,tb) -> "  (" L.++ (show ta) L.++ ", " L.++ (show $ tb) L.++ "),") $ makeAxisFileInpSer ww im)
                    PAR -> (L.map (\(ta,tb) -> "  (" L.++ (show ta) L.++ ", " L.++ (show $ tb) L.++ "),") $ makeAxisFileInpPar pw ww im)
      fileEnd =   (L.init fileData) L.++
                  [(L.init $ L.last fileData)] L.++ -- remove comma
                  ["  ]\n"]
      file = fileStart L.++ fileEnd

      -- pathWithoutName = L.reverse $ dropWhile (\c -> (c /= '/') && (c /= '\\')) $ L.reverse rockyImageOrgPath
      -- path' = pathWithoutName L.++ moduleName L.++ ".hs"
      path' = "./" L.++ moduleName L.++ ".hs"
  writeFile path' (unlines file)


lowercaseFirst :: String -> String
lowercaseFirst (x:xs) = toLower x : xs
lowercaseFirst [] = []

uppercaseFirst :: String -> String
uppercaseFirst (x:xs) = toUpper x : xs
uppercaseFirst [] = []