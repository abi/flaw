module Display (display) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
 
import Cube

--- Always keep the z coord 1 since this is a 2D game
points :: [(GLfloat,GLfloat,GLfloat)]
points = [(0, 0.5, 1), (0, 0, 1), (0, -0.5, 1)]
-- map (\k -> (sin(2*pi*k/12),cos(2*pi*k/12),0.0))  [1..12]
 
display = do 
  clear [ColorBuffer]
  mapM_ (\(x,y,z) -> preservingMatrix $ do
    color $ Color3 x y z
    translate $ Vector3 x y z
    cube (0.1::GLfloat)
    ) points
  flush