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
import Data.IORef (IORef, newIORef)
import Graphics.Rendering.DLP (DlpEncoding(..), DlpEye(..), DlpState, drawDlp, initDlp, showEye')
import Graphics.Rendering.OpenGL.GL (ClearBuffer(..), Color3(..), ComparisonFunction(Less), PrimitiveMode(..), Vector3(..), Vertex3(..), GLfloat, ($=), ($~!), clear, color, get, loadIdentity, preservingMatrix, renderPrimitive, rotate, translate, vertex)
import Graphics.UI.GLUT (DisplayCallback, DisplayMode(..), IdleCallback, createWindow, depthFunc, displayCallback, fullScreen, getArgsAndInitialize, idleCallback, initialDisplayMode, mainLoop, postRedisplay, swapBuffers)


-- | The main action.
main :: IO ()
main =
  do
    putStrLn "DLP Stereo OpenGL Example:"
    putStrLn "    Use the --fullscreen flag to run in full screen mode."
    putStrLn "    Use the --mono flag to run in monoscopic mode."
    (_, arguments) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _ <- createWindow "DLP Stereo OpenGL Example"
    depthFunc $= Just Less 
    when ("--fullscreen" `elem` arguments) fullScreen
    dlp <- initDlp
    angle <- newIORef 0
    displayCallback $= display ("--mono" `notElem` arguments) dlp angle
    idleCallback $= Just (idle angle)
    mainLoop


-- | The idle callback.
idle :: IORef GLfloat -> IdleCallback
idle angle =
  do
    angle $~! (+ 0.1)
    -- The idle callback must force redisplay for frame-sequential encoding.
    postRedisplay Nothing


-- | Draw rotating cubes.
display :: Bool -> IORef DlpState -> IORef GLfloat -> DisplayCallback
display stereo dlp angle =
  do
    -- Frame-sequential encoding is usually the easiest to use.
    let encoding = if stereo then FrameSequential else LeftOnly
    -- Determine whether to draw the view for the left of right eye.
    leftFrame <- showEye' LeftDlp encoding dlp
    angle' <- get angle
    -- Compute how to shift the view, depending on for which eye to draw.
    let offset = if leftFrame then 0.05 else -0.05 :: GLfloat
    clear [ColorBuffer, DepthBuffer]
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
    -- After all of the rendering actions, draw the colored DLP reference line just before swapping framebuffers.
    drawDlp encoding dlp
    swapBuffers


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
