module Main (
  main
) where


import Data.IORef (IORef)
import Graphics.Rendering.DLP (DlpEncoding(..), DlpEye(..), DlpState, drawDlp, initDlp, showEye')
import Graphics.Rendering.OpenGL.GL (GLfloat)
import Graphics.Rendering.OpenGL.GL.BeginEnd (PrimitiveMode(Quads), renderPrimitive)
import Graphics.Rendering.OpenGL.GL.Framebuffer (ClearBuffer(..), clear)
import Graphics.Rendering.OpenGL.GL.PerFragment (ComparisonFunction(Less), depthFunc)
import Graphics.Rendering.OpenGL.GL.StateVar (($=))
import Graphics.Rendering.OpenGL.GL.Tensor (Vertex3(..))
import Graphics.Rendering.OpenGL.GL.VertexSpec (vertex)
import Graphics.UI.GLUT.Begin (mainLoop)
import Graphics.UI.GLUT.Callbacks.Global (idleCallback)
import Graphics.UI.GLUT.Callbacks.Window (DisplayCallback, displayCallback)
import Graphics.UI.GLUT.Initialization (DisplayMode(..), getArgsAndInitialize, initialDisplayMode)
import Graphics.UI.GLUT.Window (createWindow, postRedisplay, swapBuffers)


main :: IO ()
main =
  do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "DLP Stereo OpenGL Example"
    dlp <- initDlp
    displayCallback $= display dlp
    idleCallback $= Just (postRedisplay Nothing)
    depthFunc $= Just Less 
    mainLoop


display :: IORef DlpState -> DisplayCallback
display dlp =
  do
    leftFrame <- showEye' LeftDlp FrameSequential dlp
    let
      vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
      vertex3f (x, y, z) = vertex $ Vertex3 x y z
      offset = if leftFrame then -0.25 else 0.25
    clear [ColorBuffer, DepthBuffer]
    renderPrimitive Quads
      $ mapM_ vertex3f
      [
        (offset - 0.5, -0.5, 0.5)
      , (offset - 0.5,  0.5, 0.5)
      , (offset + 0.5,  0.5, 0.5)
      , (offset + 0.5, -0.5, 0.5)
      ]
    drawDlp FrameSequential dlp
    swapBuffers
