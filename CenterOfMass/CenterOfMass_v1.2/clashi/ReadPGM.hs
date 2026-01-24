module ReadPGM where
import Clash.Prelude
import Common
import System.IO
import Data.Char
import qualified Data.List as L
import qualified Data.List.Split as S -- cabal install split

-- Strip comments and whitespace
stripComments :: [String] -> [String]
stripComments = filter (\line -> not (null line) && (notDashHead line))
  where
    notDashHead ('#':_) = False
    notDashHead _ = True

readPGM :: (Read a, Num a) => FilePath -> IO ([[a]])
readPGM path = do
  content <- readFile path
  let ls = stripComments (lines content)
      (sP2:sDims:sMaxVal:sPixels) = case ls of
        ("P2\r":_) -> ls
        ("P2\r\r":_) -> ls
        ("P2\r\n":_) -> ls
        ("P2\n":_) -> ls
        ("P2":_) -> ls
        (format:_) -> error $ "only P2 format supported, format: " L.++ (show format)
      [w,h] = L.map read (words sDims) :: [Int]
      maxVal = read sMaxVal :: Int
      pixels = L.map read (words (unwords sPixels)) :: (Read a, Num a) => [a]
      image = S.chunksOf w pixels
  putStrLn $ L.concat ["done reading,\n(w,h): ", show (w,h), "\nmaxVal: ", show maxVal]
  return image


