module Main (
  main
) where


import Data.IORef (IORef, newIORef)
import Data.Word (Word32)
import Graphics.Rendering.OpenGL.GL (GLfloat)
import Graphics.Rendering.OpenGL.GL.BeginEnd (PrimitiveMode(Quads), renderPrimitive)
import Graphics.Rendering.OpenGL.GL.CoordTrans (Position(..), Size(..), viewport)
import Graphics.Rendering.OpenGL.GL.DataType (DataType(UnsignedInt8888Rev))
import Graphics.Rendering.OpenGL.GL.Framebuffer (ClearBuffer(..), clear)
import Graphics.Rendering.OpenGL.GL.PerFragment (ComparisonFunction(Less), depthFunc)
import Graphics.Rendering.OpenGL.GL.PixelData (PixelData(..))
import Graphics.Rendering.OpenGL.GL.PixelFormat (PixelFormat(BGRA))
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization (drawPixels)
import Graphics.Rendering.OpenGL.GL.RasterPos (windowPos)
import Graphics.Rendering.OpenGL.GL.StateVar (($=), ($~!), get)
import Graphics.Rendering.OpenGL.GL.Tensor (Vertex2(..), Vertex3(..))
import Graphics.Rendering.OpenGL.GL.VertexSpec (vertex)
import Graphics.UI.GLUT.Begin (mainLoop)
import Graphics.UI.GLUT.Callbacks.Global (idleCallback)
import Graphics.UI.GLUT.Callbacks.Window (DisplayCallback, displayCallback)
import Graphics.UI.GLUT.Initialization (DisplayMode(..), getArgsAndInitialize, initialDisplayMode)
import Graphics.UI.GLUT.Window (createWindow, postRedisplay, swapBuffers)

import qualified Data.Vector.Storable as V (Vector, fromList, unsafeWith)


initDLP :: IO (IORef Bool)
initDLP = newIORef False


drawDLP :: IORef Bool -> IO ()
drawDLP leftFrame =
  do
    (Position x0 y0, Size w _) <- get viewport
    leftFrame' <- get leftFrame
    let
      red       = 0x00FF0000
      greenBlue = 0x0000FFFF
      pixels :: V.Vector Word32
      pixels =
        V.fromList
          $ replicate (fromIntegral w)
          $ if leftFrame' then red else greenBlue
    windowPos $ Vertex2 x0 y0
    V.unsafeWith pixels
      $ drawPixels (Size w 1)
      . PixelData BGRA UnsignedInt8888Rev
    leftFrame $~! not


main :: IO ()
main =
  do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "DLP Stereo OpenGL Example"
    leftFrame <- initDLP
    displayCallback $= display leftFrame
    idleCallback $= Just (postRedisplay Nothing)
    depthFunc $= Just Less 
    mainLoop


display :: IORef Bool -> DisplayCallback
display leftFrame =
  do
    leftFrame' <- get leftFrame
    let
      vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
      vertex3f (x, y, z) = vertex $ Vertex3 x y z
      offset = if leftFrame' then -0.05 else 0.05
    clear [ColorBuffer, DepthBuffer]
    renderPrimitive Quads
      $ mapM_ vertex3f
      [
        (offset - 0.5, -0.5, 0.5)
      , (offset - 0.5,  0.5, 0.5)
      , (offset + 0.5,  0.5, 0.5)
      , (offset + 0.5, -0.5, 0.5)
      ]
    drawDLP leftFrame
    swapBuffers
