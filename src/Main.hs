module Main where


import Control.Applicative ((<$>), (<*>))
import Data.IORef
import Data.Maybe (catMaybes)
import Data.Word
import Debug.Trace (trace)
import Graphics.UI.GLUT

import qualified Data.Vector.Storable as V


main :: IO ()
main =
  do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    frameNo <- newIORef 1
    _window <- createWindow "Parallel Planes"
    depthFunc $= Just Less 
    displayCallback $= display frameNo
    idleCallback $= Just idle
    mainLoop


idle :: IdleCallback
idle = postRedisplay Nothing


display :: IORef Int -> DisplayCallback
display frameNo =
  do
    (Position x0 y0, Size w _) <- get viewport
    frameNo $~! (+1)
    frameNo' <- get frameNo
    let
      offset = if frameNo' `mod` 2 == 0 then 0.1 else 0
    clear [ColorBuffer, DepthBuffer]
    renderPrimitive Quads
      $ sequence_
      [
        vertex3f (offset - 0.5, -0.5, 0.5)
      , vertex3f (offset - 0.5,  0.5, 0.5)
      , vertex3f (offset + 0.5,  0.5, 0.5)
      , vertex3f (offset + 0.5, -0.5, 0.5)
      ]
    let
      pxls :: V.Vector Word32
      pxls = V.fromList $ replicate (fromIntegral w) $ if frameNo' `mod` 2 == 0 then red else greenBlue
    windowPos $ Vertex2 x0 y0
    V.unsafeWith pxls $ \ptr ->
      drawPixels (Size w 1) $ PixelData BGRA UnsignedInt8888Rev ptr
    swapBuffers


red :: Word32
red = 0x00FF0000


greenBlue :: Word32
greenBlue = 0x0000FFFF


color3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
color3f (r, g, b) = color $ Color3 r g b


vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
