{-|
Module      :  Graphics.Rendering.DLP
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Functions for using DLP stereo with 3-D Ready Sync projectors and OpenGL.  This uses the specification \<<http://lists.gnu.org/archive/html/bino-list/2013-03/pdfz6rW7jUrgI.pdf>\> and is based on the implementation for the stereo movie viewer Bino \<<http://git.savannah.gnu.org/cgit/bino.git/tree/src/video_output.cpp?id=bino-1.6.1#n1389>\>.  In particular, note that this technique does not require a graphics card that supports @GL_STEREO@.

Here is a skeletal example illustrating the use of frame-sequential DLP:

@
main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _ <- createWindow \"DLP Stereo OpenGL Example\"
  depthFunc $= Just Less 
  dlp <- initDlp FrameSequential                                         -- Initialize the DLP state.
  displayCallback $= display dlp                                         -- The display callback needs the DLP state.
  idleCallback $= Just (postRedisplay Nothing)                           -- The idle callback must force redisplay for frame-sequential encoding.
  mainLoop

display :: IORef DlpState -> DisplayCallback
display dlp = do
  clear [ColorBuffer, DepthBuffer]
  isLeftEye <- showEye' LeftDlp dlp                                      -- Determine whether to draw the view for the left or right eye.
  translate $ Vector3 (if isLeftEye then -0.05 else 0.05 :: GLfloat) 0 0 -- Shift the view slightly, depending on for which eye to draw.
  renderPrimitive . . .                                                  -- All of the rendering actions go here.
  drawDlp dlp                                                            -- Draw the colored DLP reference line just before swapping framebuffers.
  swapBuffers
@

This code has been validated with the following configuration of hardware and software:

* Optoma ML550 WXGA 500 Lumen 3D Ready Portable DLP LED Projector, running 120 Hz at 1024x768 resolution

* Optoma ZD302 DLP Link Active Shutter 3D Glasses

* Ubuntu 15.04, 64-bit

* NVIDIA Driver Version 340.93, with @xorg.conf@ option @Stereo@ set to @8@

* GHC 7.6.3

* OpenGL == 2.8.0.0

* GLUT == 2.4.0.0
-}


module Graphics.Rendering.DLP (
-- * DLP State and Encoding 
  DlpEncoding(..)
, DlpState
, initDlp
, drawDlp
-- * Active Frame(s)
, DlpEye(..)
, showEye
, showEye'
) where


import Control.Applicative ((<$>))
import Control.Monad (unless, when)
import Data.Bits ((.|.))
import Data.IORef (IORef, newIORef)
import Data.Word (Word32)
import Graphics.Rendering.OpenGL.GL (DataType(UnsignedInt8888Rev), PixelData(..), PixelFormat(BGRA), Position(..), Size(..), Vertex2(..), ($~!), drawPixels, get, viewport, windowPos)

import qualified Data.Vector.Storable as V (fromList, unsafeWith)


-- | The type of DLP encoding.  See the specification \<<http://lists.gnu.org/archive/html/bino-list/2013-03/pdfz6rW7jUrgI.pdf>\> for further details.
data DlpEncoding =
    SideBySide       -- ^ Side-by-side encoding, where the left image is stored to the left of the right image in the framebuffer.
  | FrameSequential  -- ^ Frame-sequential encoding, where left and right images alternate, each filling the whole framebuffer.
  | TopAndBottom     -- ^ Top-and-bottom encoding, where the top image is stored above the bottom image in the framebuffer.
  | LeftOnly         -- ^ Monoscopic with only the left eye's view.
  | RightOnly        -- ^ Monoscopic with only the right eye's view.
  | FrameAlternating -- ^ Alternating frames and color bars (/experimental/).
  deriving (Eq, Read, Show)


-- | Labels for the left and right eyes' views.
data DlpEye =
    LeftDlp  -- ^ The left eye's view.
  | RightDlp -- ^ The right eye's view.
  deriving (Eq, Read, Show)


-- | The DLP state, which tracks the sequence of frames.
data DlpState = DlpState DlpEncoding Int


-- | Initialize the DLP state.
initDlp :: DlpEncoding -> IO (IORef DlpState)
initDlp encoding = newIORef $ DlpState encoding 0


-- | Query whether to show the view from the specified eye for the current frame.  Client code should call this function to determine which views to draw into the framebuffer.
showEye :: DlpEye      -- ^ The eye in question.
        -> DlpState    -- ^ The current DLP state.
        -> Bool        -- ^ Whether the view of the specified eye should be shown for the current frame.
showEye LeftDlp  (DlpState FrameSequential  frame) = frame `mod` 2 == 0
showEye RightDlp (DlpState FrameSequential  frame) = frame `mod` 2 /= 0
showEye LeftDlp  (DlpState FrameAlternating frame) = frame `mod` 2 == 0
showEye RightDlp (DlpState FrameAlternating frame) = frame `mod` 2 /= 0
showEye RightDlp (DlpState LeftOnly         _    ) = False
showEye LeftDlp  (DlpState RightOnly        _    ) = False
showEye _        _                                 = True


-- | Query whether to show the view from the specified eye for the current frame.  Client code should call this function to determine which views to draw into the framebuffer.
showEye' :: DlpEye         -- ^ The eye in question.
         -> IORef DlpState -- ^ A reference to the current DLP state.
         -> IO Bool        -- ^ An action for determining whether the view of the specified eye should be shown for the current frame.
showEye' eye = (showEye eye <$>) . get


-- | Advance the DLP state one frame.
advanceDlp :: IORef DlpState -- ^ A reference to the current DLP state.
           -> IO ()          -- ^ An action to advance the DLP state to the next frame.
advanceDlp dlp =
  dlp $~! \(DlpState encoding frame) -> DlpState encoding $ (frame + 1) `mod` 4


-- | Color constants.
red, green, blue, cyan, magenta, yellow :: Word32
red     = 0x00FF0000
green   = 0x0000FF00
blue    = 0x000000FF
cyan    = green .|. blue
magenta = red   .|. blue
yellow  = red   .|. green


-- | Determine the correct color of the reference line for a given DLP encoding and DLP state.
dlpColor :: DlpState -> Word32
dlpColor (DlpState SideBySide       frame) = if frame `mod` 2 == 0 then red   else cyan
dlpColor (DlpState FrameSequential  frame) = if frame `mod` 4 <  2 then green else magenta
dlpColor (DlpState TopAndBottom     frame) = if frame `mod` 2 == 0 then blue  else yellow
dlpColor (DlpState LeftOnly         _    ) = undefined -- Safe because drawDlp never calls the function for this DLP mode.
dlpColor (DlpState RightOnly        _    ) = undefined -- Safe because drawDlp never acalls te function for this DLP mode.
dlpColor (DlpState FrameAlternating frame) = if frame `mod` 2 == 0 then green else magenta


-- | Determine the correct color of the reference line for a given DLP encoding and DLP state.
dlpColor' :: IORef DlpState -> IO Word32
dlpColor' = (dlpColor <$>) . get


-- | Draw the DLP reference line.  This action should be executed after all other drawing is complete, just before buffers are swapped.
drawDlp :: IORef DlpState -- ^ A reference to the current DLP state.
        -> IO ()          -- ^ An action to draw the DLP reference line.
drawDlp dlp =
  do
    DlpState encoding _ <- get dlp
    unless (encoding `elem` [LeftOnly, RightOnly]) $ do
      (Position x0 y0, Size w h) <- get viewport
      color <- dlpColor' dlp
      let
        pixels = V.fromList $ replicate (fromIntegral w) color
        drawLine = V.unsafeWith pixels $ drawPixels (Size w 1) . PixelData BGRA UnsignedInt8888Rev
      windowPos $ Vertex2 x0 y0
      drawLine
      when (encoding == TopAndBottom) $ do
        windowPos $ Vertex2 x0 $ y0 + h `div` 2
        drawLine
    advanceDlp dlp
