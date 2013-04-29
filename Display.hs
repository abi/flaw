module Display (display, idle) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Cube
--idleCallback $= Just (idle ax ay)

radians a = (a / 180) * pi

display dx dy shipRotate asteriods = do 
  clear [ColorBuffer]
  dx' <- get dx
  dy' <- get dy
  a  <- get shipRotate
  asters <- get asteriods
  mapM_ (\(x,y,z,s) -> preservingMatrix $ do
    color $ Color3 x y z
    translate $ Vector3 (x-dx') (y-dy') z
    cube s
    ) asters
  preservingMatrix $ do
    rotate a $ Vector3 0 0 (1::GLfloat)
    cube (0.03::GLfloat)
  flush

idle acc a dx dy vx vy ax ay = do
  a' <- get a
  acc' <- get acc
  dx' <- get dx
  dy' <- get dy
  vx' <- get vx
  vy' <- get vy
  ax' <- get ax
  ay' <- get ay
  dx $=! (dx' + vx')
  dy $=! (dy' + vy')
  print acc'
  vx $=! (vx' + (cos $ radians a') * 0.000001)
  vy $=! (vy' + (sin $ radians a') * 0.000001)
  postRedisplay Nothing