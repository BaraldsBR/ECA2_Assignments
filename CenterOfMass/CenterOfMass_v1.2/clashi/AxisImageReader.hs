module AxisImageReader where
import Clash.Prelude
import Common
import Axi
import qualified Data.List as L
import Clash.Sized.Vector as SV
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (catMaybes)

readAxisFile :: (Read a, Num a, KnownNat n) => FilePath -> IO [(Maybe (Axi4Stream a (BitVector n)), Bool)]
readAxisFile f = do
    contents <- readFile f
    -- Only filter out the invalid lines in the file, keep the Nothings for AXIS
    return $ catMaybes $ L.map parseAxi4Stream $ lines contents

parseAxi4Stream
  :: forall a n . (Read a, Num a, KnownNat n)
  => String
  -> Maybe (Maybe (Axi4Stream a (BitVector n)), Bool)
parseAxi4Stream s =
    case words s of
      [_, _, tdata, tlast, tkeep, tready] ->
        case (reads tdata :: [(a, String)], reads tlast :: [(Bool, String)], reads tkeep ::  [([Bit], String)], reads tready :: [(Bool, String)]) of
          ([(tdata', _)], [(tlast', _)], [(tkeep', _)], [(tready', _)]) ->
            Just (Just $ Axi4Stream {tData  = tdata' :: a, tLast  = tlast' :: Bool, tKeep = v2bv $ SV.unsafeFromList tkeep'}, tready' :: Bool)
          _ -> Nothing
      [_, tready] ->
        case (reads tready :: [(Bool, String)]) of
          [(tready', _)] ->
            Just (Nothing, tready' :: Bool)
          _ -> Nothing
      _ -> Nothing


readAxisFilePar :: forall a n. (Read a, Num a, KnownNat n  -- Number of parallel units
  )
  => FilePath
  -> IO [(Maybe (Axi4Stream (Vec n a) (BitVector n)), Bool)]
readAxisFilePar f = do
    contents <- readFile f
    -- Only filter out the invalid lines in the file, keep the Nothings for AXIS
    return $ catMaybes $ L.map parseAxi4StreamPar $ lines contents

parseAxi4StreamPar :: forall a n. (Read a, Num a, KnownNat n  -- Number of parallel units
  )
  => String
  -> Maybe (Maybe (Axi4Stream (Vec n a) (BitVector n)), Bool)
parseAxi4StreamPar s =
    case words s of
      [_, _, tdata, tlast, tkeep, tready] ->
        case (reads tdata :: [([a], String)], reads tlast :: [(Bool, String)], reads tkeep :: [([Bit], String)], reads tready :: [(Bool, String)]) of
          ([(tdata', _)], [(tlast', _)], [(tkeep', _)], [(tready', _)]) ->
            Just (Just $ Axi4Stream {tData  = SV.unsafeFromList tdata', tLast  = tlast' :: Bool, tKeep = v2bv $ SV.unsafeFromList tkeep'}, tready' :: Bool)
          _ -> Nothing
      [_, tready] ->
        case (reads tready :: [(Bool, String)]) of
          [(tready', _)] ->
            Just (Nothing, tready' :: Bool)
          _ -> Nothing
      _ -> Nothing
