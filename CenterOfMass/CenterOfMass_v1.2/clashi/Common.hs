{-# LANGUAGE CPP #-}
module Common where
import Clash.Prelude
import qualified Data.List as L
import qualified Data.List.Split as S -- cabal install split
import Text.Read (Read (..), ReadPrec)

type Pixel = Unsigned 8
threshold = 128

imagePath :: FilePath
imagePath = "../images/"

lightHouseImgPath :: FilePath
lightHouseImgPath = "../images/LightHouse.pgm"

rockyImgPath :: FilePath
rockyImgPath = "../images/Rocky.pgm"

-----------------------------------------------------------------------------------------
-- helper functions below
-----------------------------------------------------------------------------------------

-- The wf function takes an image (vector of vector of grayscale pixels) and prints it to a file
--  the filename and path are specified above.
wf :: (Show a, Ord a)
  => FilePath
  -> Vec n1 (Vec n2 a)
  -> IO ()
wf path im = writeFile path (pre L.++ printimage i)
  where
    pre = "P2\r\n" L.++ (show w) L.++ " " L.++ (show h) L.++ "\r\n" L.++ mx L.++ "\r\n"
    i = toList $ map toList im
    w = L.length ((L.!!) i 0) -- length of first row is image width
    h = L.length i
    mx = show $ L.maximum $ L.map L.maximum i

-- The blocks2D function takes an image (vector of vector of grayscale pixels) and chops
--  it into different blocks. for exampe:
-- blocks2D d2 im = out
-- ww = window width = window height
blocks2D :: (KnownNat n1, KnownNat n2, (n1 * m1) ~ (n2 * m2))
  => SNat m2                      -- blockwidth = blockheight (dx where x is a number, for example d8)
  -> Vec m1 (Vec (n1 * m2) a)     -- image (Vector of Vectors containg values)
  -> Vec n2 (Vec m2 (Vec m2 a))   -- Vector containing the blocks (which are Vectors of Vectors)
blocks2D ww xss = im3 where
  im0 = map (unconcat ww) xss
  im1 = transpose im0
  im2 = concat im1
  im3 = unconcat ww im2

-- The unblocks2D function is the reversed of the blocks2D function and constructs an image
--  from a vector of blocks it needs the height of the resulting image, for exampe:
-- unblocks2D d2 bl = out
-- (larger example at the bottom of the file)
-- h = height of resulting image (h = 10? then d10)
unblocks2D :: (KnownNat n1, KnownNat n2, (n3 * m1) ~ (n1 * n2))
  => SNat n2                      -- height of original image, y, in dy, so if the height was 128, then d128
  -> Vec n3 (Vec m1 (Vec m2 a))   -- Vector containing the blocks (which are Vectors of Vectors)
  -> Vec n2 (Vec (n1 * m2) a)     -- image (Vector of Vectors containg values)
unblocks2D h xsss = im3 where
  im0 = concat xsss
  im1 = unconcat h im0
  im2 = transpose im1
  im3 = map concat im2

-- The addBorders functions takes a Vector of blocks and puts zeros at the borders of every block
addBorders :: (KnownNat n, Num a)
  => a
  -> Vec n2 (Vec n3 (Vec n a))
  -> Vec n2 (Vec ((n3 + 1) + 1) (Vec ((n + 1) + 1) a))
addBorders c xsss = xsss1 where
  xsss0 = map (\xss -> zs:>xss ++ (zs:>Nil)) xsss
  xsss1 = map (map (\xs -> c:>xs ++ (c:>Nil))) xsss0
  zs    = repeat c

printimage :: (Show a) => [[a]] -> [Char]
printimage [] = []
printimage (xs:xss) = (printimage' xs) L.++ "\r\n" L.++ printimage xss

printimage' :: (Show a) => [a] -> [Char]
printimage' [] = []
printimage' (x:xs) = (show x) L.++ " " L.++ printimage' xs



instance Read Bit where
  readPrec = fromIntegral <$> (readPrec :: ReadPrec Integer)

removeExtension :: FilePath -> FilePath
removeExtension path = L.reverse (L.drop 1 $ dropWhile (/='.') (L.reverse path))



wfL :: (Show a, Ord a)
  => FilePath   -- path, including the outputfile name
  -> [[a]]      -- imag
  -> IO ()
wfL path i = writeFile path (pre L.++ printimage i)
  where
    pre = "P2\r\n" L.++ (show w) L.++ " " L.++ (show h) L.++ "\r\n" L.++ mx L.++ "\r\n"
    w = L.length (i L.!! 0) -- length of first row is image width
    h = L.length i
    mx = show $ L.maximum $ L.map L.maximum i

blocks2DL ::
  Int         -- window width and height
  -> [[a]]    -- image as a list of lists
  -> [[[a]]]  -- a list containing the blocks (images) (which are list of lists)
blocks2DL ww xss = im3 where  -- This function could be a one-liner but this is more readable
  im0 = L.map (S.chunksOf ww) (cropImgL ww xss)
  im1 = L.transpose im0
  im2 = L.concat im1
  im3 = S.chunksOf ww im2

cropImgL ::
  Int       -- window width and height
  -> [[a]]  -- image
  -> [[a]]  -- image cropped
cropImgL ww img = img3 where
  h = L.length img
  w = L.length fr
  (fr:_) = img

  loR = mod h ww
  dropStartRows = div loR 2
  h' = h - loR

  loC = mod w ww
  dropStartCols = div loC 2
  w' = w - loC
  img0 = L.drop dropStartRows img
  img1 = L.take h' img0
  img2 = L.map (L.drop dropStartCols) img1
  img3 = L.map (L.take w') img2

unblocks2DL ::
  Int         -- height over orignal image
  -> [[[a]]]  -- list containing the blocks (images) (which are list of lists)
  ->  [[a]]   -- image as a list of lists
unblocks2DL h xsss = im3 where   -- This function could be a one-liner but this is more readable
  im0 = L.concat xsss
  im1 = S.chunksOf h im0
  im2 = L.transpose im1
  im3 = L.map L.concat im2

addBordersL :: Num a
  => a        -- grayscale of border
  -> [[[a]]]  -- original blocks
  -> [[[a]]]  -- blocks with borders
addBordersL c xsss = xsss1 where
  xsss0 = L.map (\xss -> zs:xss L.++ [zs]) xsss
  xsss1 = L.map (L.map (\xs -> c:xs L.++ [c])) xsss0
  zs = L.replicate w c
  w = (L.map L.length xsss) L.!! 0

changePixelInImageL :: (Num a, Enum a, Eq a)
  => p     -- new value
  -> (a,a) -- (row,collumn)
  -> [[p]] -- list of list of values
  -> [[p]] -- list of list of values with updated value
changePixelInImageL p (r,c) im = [if r == j then (changePixelInRowL p c row) else row | (row,j) <- L.zip im [0..]]

changePixelInRowL :: (Num a, Enum a, Eq a)
  => p      -- new value
  -> a      -- collumn coordinate
  -> [p]    -- list ov values (row)
  -> [p]    -- list of value with updated value
changePixelInRowL p c row = [if c == i then p else e| (e,i) <- L.zip row [0..]]