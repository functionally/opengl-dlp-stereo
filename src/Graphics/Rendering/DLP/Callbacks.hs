{-# LANGUAGE RecordWildCards #-}


module Graphics.Rendering.DLP.Callbacks (
  DlpDisplay(..)
, DlpDisplayCallback
, dlpDisplayCallback
) where


import Data.Default (Default(..))
import Graphics.Rendering.DLP (DlpEncoding(FrameSequential), DlpEye(..), drawDlp, initDlp, showEye, whichView)
import Graphics.Rendering.OpenGL (ClearBuffer(..), SettableStateVar, ($=!), clear, get, makeSettableStateVar, viewport)
import Graphics.UI.GLUT (DisplayCallback, displayCallback, swapBuffers)


data DlpDisplay =
  DlpDisplay
  {
    dlpEncoding :: DlpEncoding
  , preDisplay  :: DisplayCallback
  , doDisplay   :: DlpDisplayCallback
  , postDisplay :: DisplayCallback
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


type DlpDisplayCallback = DlpEye -> IO ()


dlpDisplayCallback :: SettableStateVar DlpDisplay
dlpDisplayCallback =
  makeSettableStateVar setDlpDisplayCallback
    where
      setDlpDisplayCallback :: DlpDisplay -> IO ()
      setDlpDisplayCallback DlpDisplay{..} =
        do
          dlpRef <- initDlp dlpEncoding
          displayCallback $=!
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
