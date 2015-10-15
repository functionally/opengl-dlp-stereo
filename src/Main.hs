module Main (
  main
) where


import Control.Monad (when)
import Data.IORef (IORef, newIORef)
import Graphics.Rendering.DLP (DlpEncoding(..), DlpEye(..), DlpState, drawDlp, initDlp, showEye')
import Graphics.Rendering.OpenGL.GL (GLfloat)
import Graphics.Rendering.OpenGL.GL.BeginEnd (PrimitiveMode(Lines, Quads), renderPrimitive)
import Graphics.Rendering.OpenGL.GL.CoordTrans (loadIdentity, preservingMatrix, rotate, translate)
import Graphics.Rendering.OpenGL.GL.Framebuffer (ClearBuffer(..), clear)
import Graphics.Rendering.OpenGL.GL.PerFragment (ComparisonFunction(Less), depthFunc)
import Graphics.Rendering.OpenGL.GL.StateVar (($=), ($~!), get)
import Graphics.Rendering.OpenGL.GL.Tensor (Vector3(..), Vertex3(..))
import Graphics.Rendering.OpenGL.GL.VertexAttributes (Color3(..))
import Graphics.Rendering.OpenGL.GL.VertexSpec (color, vertex)
import Graphics.UI.GLUT.Begin (mainLoop)
import Graphics.UI.GLUT.Callbacks.Global (IdleCallback, idleCallback)
import Graphics.UI.GLUT.Callbacks.Window (DisplayCallback, displayCallback)
import Graphics.UI.GLUT.Initialization (DisplayMode(..), getArgsAndInitialize, initialDisplayMode)
import Graphics.UI.GLUT.Window (createWindow, fullScreen, postRedisplay, swapBuffers)


main :: IO ()
main =
  do
    (_, arguments) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "DLP Stereo OpenGL Example"
    when ("--fullscreen" `elem` arguments) fullScreen
    dlp <- initDlp
    angle <- newIORef 0
    displayCallback $= display dlp angle
    idleCallback $= Just (idle angle)
    depthFunc $= Just Less 
    mainLoop


idle :: IORef GLfloat -> IdleCallback
idle angle =
  do
    angle $~! (+ 0.1)
    postRedisplay Nothing


display :: IORef DlpState -> IORef GLfloat -> DisplayCallback
display dlp angle =
  do
    leftFrame <- showEye' LeftDlp FrameSequential dlp
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
    drawDlp FrameSequential dlp
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
