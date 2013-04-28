module Display (display, idle) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Cube
 

--idleCallback $= Just (idle ax ay)

display vx vy shipRotate asteriods = do 
  clear [ColorBuffer]
  vx_ <- get vx
  vy_ <- get vy
  a  <- get shipRotate
  asters <- get asteriods
  mapM_ (\(x,y,z) -> preservingMatrix $ do
    color $ Color3 x y z
    translate $ Vector3 (x-vx_) (y-vy_) z
    cube (0.1::GLfloat)
    ) asters
  preservingMatrix $ do
    rotate a $ Vector3 0 0 (1::GLfloat)
    cube (0.2::GLfloat)
  flush

idle vx vy ax ay = do
  vx' <- get vx
  vy' <- get vy
  ax' <- get ax
  ay' <- get ay
  vx $=! (vx' + ax')
  vy $=! (vy' + ay')
  postRedisplay Nothing