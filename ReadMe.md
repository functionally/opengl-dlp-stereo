Functions for using DLP stereo with 3-D Ready Sync projectors and OpenGL
========================================================================

This Haskell package contains functions for rendering 3D stereo using DLP 3-D Ready Sync projectors and active-shutter stereo glasses.  It also includes [a sample application](https://youtu.be/l3rZbMB2XjM) and notes regarding hardware setup for DLP.  In particular, note that this technique does not require a graphics card that supports `GL_STEREO`.

The code conforms to the specification <<http://lists.gnu.org/archive/html/bino-list/2013-03/pdfz6rW7jUrgI.pdf>> and is based on the implementation for the stereo movie viewer Bino <<http://git.savannah.gnu.org/cgit/bino.git/tree/src/video_output.cpp?id=bino-1.6.1#n1389>>.


Skeletal example illustrating the use of frame-sequential DLP
-------------------------------------------------------------

```haskell
main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _ <- createWindow "DLP Stereo OpenGL Example"
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
```


Notes on hardware and software
------------------------------

This code has been validated with the following configuration of hardware and software:

*   Optoma ML550 WXGA 500 Lumen 3D Ready Portable DLP LED Projector, running 120 Hz at 1024x768 resolution
*   Optoma ZD302 DLP Link Active Shutter 3D Glasses
*   Ubuntu 15.04, 64-bit
*   NVIDIA Driver Version 340.93, with `xorg.conf` option `Stereo` set to `8`
*   GHC 7.6.3
*   OpenGL == 2.8.0.0
*   GLUT == 2.4.0.0
