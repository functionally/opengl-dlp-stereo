{-|
Module      :  Graphics.Rendering.DLP.Callbacks
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

A simple callback for using DLP stereo with 3-D Ready Sync projectors and OpenGL.  See "Graphics.Rendering.DLP" for more primitive functions for using DLP and for notes on DLP specifications and hardware.

Here is a skeletal example illustrating the use of frame-sequential DLP:

@
main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _ <- createWindow \"DLP Stereo OpenGL Example\"
  depthFunc $= Just Less 
  idleCallback $= Just (postRedisplay Nothing)
  -- Use frame-sequential DLP encoding.
  dlpDisplayCallback $= def {dlpEncoding = FrameSequential, doDisplay = display}
  mainLoop

display :: DlpDisplayCallback
display eye = do
  -- Shift the view slightly, depending on for which eye to draw.
  translate $ Vector3 (if eye == LeftDlp then -0.05 else 0.05 :: GLfloat) 0 0 
  -- All of the rendering actions go here.
  renderPrimitive . . .
@
-}


{-# LANGUAGE RecordWildCards #-}


module Graphics.Rendering.DLP.Callbacks (
  -- * Callbacks
  DlpDisplay(..)
, DlpDisplayCallback
, dlpDisplayCallback
) where


import Data.Default (Default(..))
import Data.IORef (IORef)
import Graphics.Rendering.DLP (DlpEncoding(FrameSequential, QuadBuffer), DlpEye(..), DlpState, drawDlp, initDlp, showEye, whichView)
import Graphics.Rendering.OpenGL (BufferMode(BackLeftBuffer, BackRightBuffer), ClearBuffer(..), SettableStateVar, ($=!), clear, drawBuffer, get, makeSettableStateVar, viewport)
import Graphics.UI.GLUT (DisplayCallback, displayCallback, swapBuffers)


-- | The type of DLP encoding and the actions associated with the display callback.
data DlpDisplay =
  DlpDisplay
  {
    dlpEncoding :: DlpEncoding        -- ^ The DLP encoding.  The default is frame-sequential DLP encoding.
  , preDisplay  :: DisplayCallback    -- ^ The display action to perform before the views from the eye(s) are displayed.  The default is to clear the color and depth buffers.
  , doDisplay   :: DlpDisplayCallback -- ^ The action for displaying the view from an eye.  The default is to do nothing.
  , postDisplay :: DisplayCallback    -- ^ The display action to perform after the views from the eye(s) are displayed.  The default is to do nothing.
  }

instance Default DlpDisplay where
  def =
    DlpDisplay
    {
      dlpEncoding = FrameSequential
    , preDisplay  = clear [ColorBuffer, DepthBuffer]
    , doDisplay   = const $ return ()
    , postDisplay = return ()
    }


-- | A callback for displaying using DLP.
type DlpDisplayCallback =  DlpEye          -- ^ The eye to be displayed.
                        -> DisplayCallback -- ^ The action to display the view from the eye in question.


-- | Set a DLP display callback.  Note that 'preDisplay' is executed first, then 'doDisplay' is executed to display the views for whichever eye(s) need displaying, and finally 'postDisplay' is executed before 'Graphics.UI.GLUT.swapBuffers' is executed.  The viewport is adjusted appropriately each time before `doDisplay' is executed.
dlpDisplayCallback :: SettableStateVar DlpDisplay
dlpDisplayCallback =
  makeSettableStateVar setDlpDisplayCallback
    where
      setDlpDisplayCallback :: DlpDisplay -> IO ()
      setDlpDisplayCallback dlpDisplay@DlpDisplay{..} =
        do
          dlpRef <- initDlp dlpEncoding
          displayCallback $=!
            case dlpEncoding of
              QuadBuffer -> doQuad dlpRef dlpDisplay
              _          -> doDlp  dlpRef dlpDisplay


-- | Make a display callback for DLP.
doDlp :: IORef DlpState  -- ^ A reference to the DLP state.
      -> DlpDisplay      -- ^ The display information.
      -> DisplayCallback -- ^ The display callback.
doDlp dlpRef DlpDisplay{..} =
  do
    vpSaved <- get viewport
    preDisplay
    dlp <- get dlpRef
    sequence_
      [
        do
          vp <- whichView eye dlp
          viewport $=! vp
          doDisplay eye
          viewport $=! vpSaved
      |
        eye <- [LeftDlp, RightDlp]
      , showEye eye dlp
      ]
    postDisplay
    drawDlp dlpRef
    swapBuffers


-- | Make a display callback for quad buffer stereo.
doQuad :: IORef DlpState  -- ^ A reference to the DLP state.
       -> DlpDisplay      -- ^ The display information.
       -> DisplayCallback -- ^ The display callback.
doQuad dlpRef DlpDisplay{..} =
  do
    sequence_
      [
        do
          drawBuffer $=! buffer
          preDisplay
          doDisplay eye
          postDisplay
          drawDlp dlpRef
      |
        (eye, buffer) <- zip [LeftDlp, RightDlp] [BackLeftBuffer, BackRightBuffer]
      ]
    swapBuffers
