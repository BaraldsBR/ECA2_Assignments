module RunImage where
import Clash.Prelude
import Common
import ReadPGM
import AxisImageGenerator
import Data.Maybe
import Axi
import qualified Data.List as L

import CCenterOfMassXX -- Change this to your team nr.

runLightHouse :: IO ()
runLightHouse = do
  runImage SER d1 d8 lightHouseImgPath
  runImage PAR d16 d8 lightHouseImgPath


runRocky :: IO ()
runRocky = do
  runImage SER d1 d8 rockyImgPath
  runImage PAR d16 d8 rockyImgPath


runImage :: (KnownNat ww, KnownNat pw
  , (n + pw) ~ (ww * ww)
  , (pw + n) ~ (ww * ww)
  )
  => Mode     -- Serial or Parallel input stream
  -> SNat pw  -- Parallel width, ignored if Serial stream
  -> SNat ww  -- Window width
  -> FilePath -- Path to image.pgm file
  -> IO ()
runImage mode pw ww path = do
  im <- readPGM path
  let ww' = snatToNum ww
      imCropped = cropImgL ww' im
      h = L.length imCropped
      w = L.length fr
      (fr:_) = imCropped
  putStrLn $ L.concat ["Image cropped to (w,h): ", show (w,h)]
  let funOut = case mode of
                  -- SER -> let imAxis = makeAxisInpSer    ww' imCropped in simulateN @System (L.length imAxis) (mAxisCom       ww) imAxis
                  -- PAR -> let imAxis = makeAxisInpPar pw ww' imCropped in simulateN @System (L.length imAxis) (mAxisComPar pw ww) imAxis
                  SER -> let imAxis = makeAxisInpSer     8 imCropped in simulateN @System (L.length imAxis) (mAxisComSer) imAxis
                  PAR -> let imAxis = makeAxisInpPar d16 8 imCropped in simulateN @System (L.length imAxis) (mAxisComPar) imAxis
      coords = L.map tData (catMaybes $ L.map fst funOut)
  putStrLn $ L.concat ["Simulation done, output length: ", show (L.length coords)]
  let h' = h + (div h ww')*2
      w' = w + (div w ww')*2
      imWithCom = unblocks2DL h' $ addBordersL 3 $ L.zipWith (changePixelInImageL 2) coords (blocks2DL ww' ((L.map . L.map) (\i -> if i < threshold then 0 else 1) imCropped))
  putStrLn $ L.concat ["New image generated, (w',h'): ", show (w',h')]
  let path' = case mode of
                SER -> ((removeExtension path) L.++ "SerOut.pgm")
                PAR -> ((removeExtension path) L.++ "ParOut.pgm")
  wfL path' imWithCom


