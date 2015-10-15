{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Graphics.Rendering.DLP (
  DlpEncoding(SideBySide, FrameSequential, TopAndBottom, LeftOnly)
, DlpState
, initDlp
, DlpEye(..)
, showEye
, showEye'
, drawDlp
) where


import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Bits ((.|.))
import Data.IORef (IORef, newIORef)
import Data.Word (Word32)
import Graphics.Rendering.OpenGL.GL

import qualified Data.Vector.Storable as V (fromList, unsafeWith)


-- Specifications from <http://lists.gnu.org/archive/html/bino-list/2013-03/pdfz6rW7jUrgI.pdf>.

-- Implementation based on <http://git.savannah.gnu.org/cgit/bino.git/tree/src/video_output.cpp?id=bino-1.6.1#n1389>.


data DlpEncoding = SideBySide | FrameSequential | TopAndBottom | LeftOnly | Experimental
  deriving (Eq, Read, Show)


data DlpEye = LeftDlp | RightDlp
  deriving (Eq, Read, Show)


newtype DlpState = DlpState Int
  deriving (Enum, Eq, Integral, Num, Ord, Real)


initDlp :: IO (IORef DlpState)
initDlp = newIORef 0


showEye :: DlpEye -> DlpEncoding -> DlpState -> Bool
showEye LeftDlp  Experimental    = (== 0) . (`mod` 2)
showEye RightDlp Experimental    = (/= 0) . (`mod` 2)
showEye LeftDlp  LeftOnly        = const True
showEye RightDlp LeftOnly        = const False
showEye LeftDlp  FrameSequential = (== 0) . (`mod` 2)
showEye RightDlp FrameSequential = (/= 0) . (`mod` 2)
showEye _        _               = const True


showEye' :: DlpEye -> DlpEncoding -> IORef DlpState -> IO Bool
showEye' eye encoding = (showEye eye encoding <$>) . get


advanceDlp :: IORef DlpState -> IO ()
advanceDlp = ($~! ((`mod` 4) . (+ 1)))


red, green, blue, cyan, magenta, yellow :: Word32
red     = 0x00FF0000
green   = 0x0000FF00
blue    = 0x000000FF
cyan    = green .|. blue
magenta = red   .|. blue
yellow  = red   .|. green


dlpColor :: DlpEncoding -> DlpState -> Word32
dlpColor Experimental    state = if state `mod` 2 == 0 then red   else cyan
dlpColor SideBySide      state = if state `mod` 2 == 0 then red   else cyan
dlpColor FrameSequential state = if state `mod` 4 <  2 then green else magenta
dlpColor TopAndBottom    state = if state `mod` 2 == 0 then blue  else yellow
dlpColor _               _     = undefined


dlpColor' :: DlpEncoding -> IORef DlpState -> IO Word32
dlpColor' encoding = (dlpColor encoding <$>) . get


drawDlp :: DlpEncoding -> IORef DlpState -> IO ()
drawDlp LeftOnly _     = return ()
drawDlp encoding state =
  do
    (Position x0 y0, Size w h) <- get viewport
    color <- dlpColor' encoding state
    let
      pixels = V.fromList $ replicate (fromIntegral w) color
      drawLine = V.unsafeWith pixels $ drawPixels (Size w 1) . PixelData BGRA UnsignedInt8888Rev
    windowPos $ Vertex2 x0 y0
    drawLine
    when (encoding == TopAndBottom) $ do
      windowPos $ Vertex2 x0 $ y0 + h `div` 2
      drawLine
    advanceDlp state
