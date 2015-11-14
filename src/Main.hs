{-|
Module      :  Main
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Example application illustrating frame-sequential DLP.
-}


module Main (
-- * Entry Point
  main
) where


import Control.Monad (when)
import Data.Default (def)
import Data.IORef (IORef, newIORef)
import Graphics.Rendering.DLP (DlpEncoding(..), DlpEye(..))
import Graphics.Rendering.DLP.Callbacks (DlpDisplay(..), DlpDisplayCallback, dlpDisplayCallback)
import Graphics.Rendering.OpenGL.GL (Color3(..), ComparisonFunction(Less), PrimitiveMode(..), Vector3(..), Vertex3(..), GLfloat, ($=!), ($~!), color, get, loadIdentity, preservingMatrix, renderPrimitive, rotate, translate, vertex)
import Graphics.UI.GLUT (DisplayMode(..), IdleCallback, createWindow, depthFunc, fullScreen, getArgsAndInitialize, idleCallback, initialDisplayMode, mainLoop, postRedisplay)


-- | The main action.
main :: IO ()
main =
  do
    putStrLn "DLP Stereo OpenGL Example:"
    putStrLn "    Use the --fullscreen flag to run in full screen mode."
    putStrLn "    Use the --mono flag to run in monoscopic mode."
    putStrLn "    Use the --cardboard flag to run in side-by-side (Google Cardboard) mode."
    (_, arguments) <- getArgsAndInitialize
    initialDisplayMode $=! [WithDepthBuffer, DoubleBuffered]
    _ <- createWindow "DLP Stereo OpenGL Example"
    depthFunc $=! Just Less 
    when ("--fullscreen" `elem` arguments) fullScreen
    angle <- newIORef 0
    let encoding
          | "--mono"      `elem` arguments = LeftOnly
          | "--cardboard" `elem` arguments = SideBySide
          | otherwise                      = FrameSequential
    dlpDisplayCallback $=! def {dlpEncoding = encoding, doDisplay = display angle}
    idleCallback $=! Just (idle angle)
    mainLoop


-- | The idle callback.
idle :: IORef GLfloat -> IdleCallback
idle angle =
  do
    angle $~! (+ 0.1)
    postRedisplay Nothing


-- | Draw rotating cubes.
display :: IORef GLfloat -> DlpDisplayCallback
display angle eye =
  do
    angle' <- get angle
    let offset = case eye of
                   LeftDlp  ->  0.05 
                   RightDlp -> -0.05 :: GLfloat
    loadIdentity
    preservingMatrix $ do
      translate $ Vector3 offset 0 0.5
      rotate angle' $ Vector3 1 1 1
      color $ Color3 0.5 0.35 (0 :: GLfloat)
      cube 0.5
      color $ Color3 0.5 0.65 (1 :: GLfloat)
      cubeFrame 0.5
    preservingMatrix $ do
      translate $ Vector3 offset 0 0
      rotate (- angle') $ Vector3 1 1 1
      color $ Color3 0 0.35 (0.5 :: GLfloat)
      cube 0.25
      color $ Color3 1 0.65 (0.5 :: GLfloat)
      cubeFrame 0.25


-- | Make a cube.  *Source:* \<<https://wiki.haskell.org/OpenGLTutorial2>\>.
cube :: GLfloat -> IO ()
cube w =
  renderPrimitive Quads
    $ mapM_ vertex3f
    [
      ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w)
    , ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w)
    , ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w)
    , (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w)
    , ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w)
    , ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w)
    ]


-- | Make the frame of a cube.  *Source:* \<<https://wiki.haskell.org/OpenGLTutorial2>\>.
cubeFrame :: GLfloat -> IO ()
cubeFrame w =
  renderPrimitive Lines
    $ mapM_ vertex3f
    [
      ( w,-w, w), ( w, w, w)
    , ( w, w, w), (-w, w, w)
    , (-w, w, w), (-w,-w, w)
    , (-w,-w, w), ( w,-w, w)
    , ( w,-w, w), ( w,-w,-w)
    , ( w, w, w), ( w, w,-w)
    , (-w, w, w), (-w, w,-w)
    , (-w,-w, w), (-w,-w,-w)
    , ( w,-w,-w), ( w, w,-w)
    , ( w, w,-w), (-w, w,-w)
    , (-w, w,-w), (-w,-w,-w)
    , (-w,-w,-w), ( w,-w,-w)
    ]


-- | Make a vertex.
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
