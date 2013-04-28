module Display (display, idle) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Cube
 
display xTranslate shipRotate asteriods = do 
  clear [ColorBuffer]
  xT <- get xTranslate
  a  <- get shipRotate
  asters <- get asteriods
  mapM_ (\(x,y,z) -> preservingMatrix $ do
    color $ Color3 x y z
    translate $ Vector3 (x-xT) y z
    cube (0.1::GLfloat)
    ) asters
  preservingMatrix $ do
    rotate a $ Vector3 0 0 (1::GLfloat)
    cube (0.2::GLfloat)
  flush

idle xTranslate = do
  xT <- get xTranslate
  xTranslate $=! (xT + 0.0005)
  postRedisplay Nothing