module Display (initGraphics, displayWorld) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Lens
import GameObjects


initGraphics :: IO ()
initGraphics = do
  initialWindowSize $= Size 800 800
  getArgsAndInitialize
  createWindow "Flaw"
  displayCallback $= return ()

displayWorld w = do
  let s = w^.ship
  clear [ColorBuffer]
  loadIdentity
  centerView (realToFrac $ s^.pos._x) (realToFrac $ s^.pos._y) 50
  preservingMatrix $ do
    color $ Color3 1.0 0.0 (1.0 :: GLfloat)
    translate $ Vector3 (realToFrac $ s^.pos^._x) (realToFrac $ s^.pos^._y) (1 :: GLfloat)
    rotate (realToFrac $ s^.angle) $ Vector3 0 0 (1 :: GLfloat)
    translate $ Vector3 (-0.5) (-1) (0 :: GLfloat)
    renderPrimitive Quads $ vertify3 [(0,0,0), (0,2,0), (1,2,0), (1,0,0)]
  color $ Color3 0.0 1.0 (0.0 :: GLfloat)
  drawCircle 5
  flush


drawCircle :: GLdouble -> IO ()
drawCircle radius = 
  renderQuadric (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle)
                (Disk 0 radius 50 1)

vertify3 :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
vertify3 verts = sequence_ $ map (\(a,b,c) -> vertex $ Vertex3 a b c) verts 

centerView :: GLdouble -> GLdouble -> GLdouble -> IO ()
centerView x y zoom = do
  ortho2D (x - zoom) (x + zoom) (y - zoom) (y + zoom)
