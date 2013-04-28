module Display (display, idle) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
 
import Cube

--- Always keep the z coord 1 since this is a 2D game
points :: [(GLfloat,GLfloat,GLfloat)]
points = [(0, 0.5, 1), (0, 0, 1), (0, -0.5, 1),
          (1.2, 0.5, 1), (0.5, -0.4, 1), (1.7, -0.5, 1),
          (2.5, 0.5, 1), (2.4, 0, 1), (2.8, -0.5, 1),
          (3.8, 0.2, 1), (3.3, -0.3, 1), (3.2, -0.5, 1)]
-- map (\k -> (sin(2*pi*k/12),cos(2*pi*k/12),0.0))  [1..12]
 
display xTranslate shipRotate = do 
  clear [ColorBuffer]
  xT <- get xTranslate
  a  <- get shipRotate
  mapM_ (\(x,y,z) -> preservingMatrix $ do
    color $ Color3 x y z
    translate $ Vector3 (x-xT) y z
    cube (0.1::GLfloat)
    ) points
  preservingMatrix $ do
    rotate a $ Vector3 0 0 (1::GLfloat)
    cube (0.2::GLfloat)
  flush

idle xTranslate = do
  xT <- get xTranslate
  xTranslate $=! (xT + 0.0005)
  postRedisplay Nothing