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
  dlp <- initDlp                                                         -- Initialize the DLP state.
  displayCallback $= display dlp                                         -- The display callback needs the DLP state.
  idleCallback $= Just (postRedisplay Nothing)                           -- The idle callback must force redisplay for frame-sequential encoding.
  mainLoop

encoding :: DlpEncoding
encoding = FrameSequential                                               -- Frame-sequential encoding is usually easiest to code.

display :: IORef DlpState -> DisplayCallback
display dlp = do
  clear [ColorBuffer, DepthBuffer]
  isLeftEye <- showEye' LeftDlp encoding dlp                             -- Determine whether to draw the view for the left or right eye.
  translate $ Vector3 (if isLeftEye then -0.05 else 0.05 :: GLfloat) 0 0 -- Shift the view slightly, depending on for which eye to draw.
  renderPrimitive . . .                                                  -- All of the rendering actions go here.
  drawDlp encoding dlp                                                   -- Draw the colored DLP reference line just before swapping framebuffers.
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
import Control.Monad (when)
import Data.Bits ((.|.))
import Data.IORef (IORef, newIORef)
import Data.Word (Word32)
import Graphics.Rendering.OpenGL.GL (DataType(UnsignedInt8888Rev), PixelData(..), PixelFormat(BGRA), Position(..), Size(..), Vertex2(..), ($~!), drawPixels, get, viewport, windowPos)

import qualified Data.Vector.Storable as V (fromList, unsafeWith)


-- | The type of DLP encoding.  See the specification \<<http://lists.gnu.org/archive/html/bino-list/2013-03/pdfz6rW7jUrgI.pdf>\> for further details.
data DlpEncoding =
    SideBySide      -- ^ Side-by-side encoding, where the left image is stored to the left of the right image in the framebuffer.
  | FrameSequential -- ^ Frame-sequential encoding, where left and right images alternate, each filling the whole framebuffer.
  | TopAndBottom    -- ^ Top-and-bottom encoding, where the top image is stored above the bottom image in the framebuffer.
  | LeftOnly        -- ^ Monoscopic with only the left eye's view.
  | RightOnly       -- ^ Monoscopic with only the right eye's view.
  deriving (Eq, Read, Show)


-- | Labels for the left and right eyes' views.
data DlpEye =
    LeftDlp  -- ^ The left eye's view.
  | RightDlp -- ^ The right eye's view.
  deriving (Eq, Read, Show)


-- | The DLP state, which tracks the sequence of frames.
newtype DlpState = DlpState {unDlpState :: Int}


-- | Initialize the DLP state.
initDlp :: IO (IORef DlpState)
initDlp = newIORef $ DlpState 0


-- | Query whether to show the view from the specified eye for the current frame.  Client code should call this function to determine which views to draw into the framebuffer.
showEye :: DlpEye      -- ^ The eye in question.
        -> DlpEncoding -- ^ The DLP encoding.
        -> DlpState    -- ^ The current DLP state.
        -> Bool        -- ^ Whether the view of the specified eye should be shown for the current frame.
showEye LeftDlp  FrameSequential = (== 0) . (`mod` 2) . unDlpState
showEye RightDlp FrameSequential = (/= 0) . (`mod` 2) . unDlpState
showEye RightDlp LeftOnly        = const False
showEye LeftDlp  RightOnly       = const False
showEye _        _               = const True


-- | Query whether to show the view from the specified eye for the current frame.  Client code should call this function to determine which views to draw into the framebuffer.
showEye' :: DlpEye         -- ^ The eye in question.
         -> DlpEncoding    -- ^ The DLP encoding.
         -> IORef DlpState -- ^ A reference to the current DLP state.
         -> IO Bool        -- ^ An action for determining whether the view of the specified eye should be shown for the current frame.
showEye' eye encoding = (showEye eye encoding <$>) . get


-- | Advance the DLP state one frame.
advanceDlp :: IORef DlpState -- ^ A reference to the current DLP state.
           -> IO ()          -- ^ An action to advance the DLP state to the next frame.
advanceDlp = ($~! (DlpState . (`mod` 4) . (+ 1) . unDlpState))


-- | Color constants.
red, green, blue, cyan, magenta, yellow :: Word32
red     = 0x00FF0000
green   = 0x0000FF00
blue    = 0x000000FF
cyan    = green .|. blue
magenta = red   .|. blue
yellow  = red   .|. green


-- | Determine the correct color of the reference line for a given DLP encoding and DLP state.
dlpColor :: DlpEncoding -> DlpState -> Word32
dlpColor SideBySide      (DlpState state) = if state `mod` 2 == 0 then red   else cyan
dlpColor FrameSequential (DlpState state) = if state `mod` 4 <  2 then green else magenta
dlpColor TopAndBottom    (DlpState state) = if state `mod` 2 == 0 then blue  else yellow
dlpColor LeftOnly        _                = undefined -- Safe because drawDlp never calls the function for this DLP mode.
dlpColor RightOnly       _                = undefined -- Safe because drawDlp never acalls te function for this DLP mode.


-- | Determine the correct color of the reference line for a given DLP encoding and DLP state.
dlpColor' :: DlpEncoding -> IORef DlpState -> IO Word32
dlpColor' encoding = (dlpColor encoding <$>) . get


-- | Draw the DLP reference line.  This action should be executed after all other drawing is complete, just before buffers are swapped.
drawDlp :: DlpEncoding    -- ^ The DLP encoding.
        -> IORef DlpState -- ^ A reference to the current DLP state.
        -> IO ()          -- ^ An action to draw the DLP reference line.
drawDlp LeftOnly  _     = return ()
drawDlp RightOnly _     = return ()
drawDlp encoding  state =
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
