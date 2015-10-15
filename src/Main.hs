module Main (
  main
) where


import Control.Monad (when)
import Data.IORef (IORef, newIORef)
import Graphics.Rendering.DLP (DlpEncoding(..), DlpEye(..), DlpState, drawDlp, initDlp, showEye')
import Graphics.UI.GLUT


main :: IO ()
main =
  do
    putStrLn "DLP Stereo OpenGL Example:"
    putStrLn "    Use the --fullscreen flag to run in full screen mode."
    putStrLn "    Use the --mono flag to run in monoscopic mode."
    (_, arguments) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _ <- createWindow "DLP Stereo OpenGL Example"
    when ("--fullscreen" `elem` arguments) fullScreen
    dlp <- initDlp
    angle <- newIORef 0
    displayCallback $= display ("--mono" `notElem` arguments) dlp angle
    idleCallback $= Just (idle angle)
    depthFunc $= Just Less 
    mainLoop


idle :: IORef GLfloat -> IdleCallback
idle angle =
  do
    angle $~! (+ 0.1)
    postRedisplay Nothing


display :: Bool -> IORef DlpState -> IORef GLfloat -> DisplayCallback
display stereo dlp angle =
  do
    let
      encoding = if stereo then FrameSequential else LeftOnly
    leftFrame <- showEye' LeftDlp encoding dlp
    angle' <- get angle
    let
      offset :: GLfloat
      offset = if leftFrame then -0.05 else 0.05
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
    drawDlp encoding dlp
    swapBuffers


-- From <https://wiki.haskell.org/OpenGLTutorial2>.

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


-- From <https://wiki.haskell.org/OpenGLTutorial2>.

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


vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
