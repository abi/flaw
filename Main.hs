import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
 
import Bindings
 
import Data.List
import System.Random

gridSize = 6

--- Always keep the z coord 1 since this is a 2D game
genAsteriods :: Int -> StdGen -> StdGen -> [(GLfloat,GLfloat,GLfloat, GLfloat)]
genAsteriods k seed1 seed2 = zip4 xs ys (repeat 1) sizes
                              where xs = (randomlist k (-gridSize/2, gridSize/2) seed1)
                                    ys = (randomlist k (-gridSize/2, gridSize/2) seed2)
                                    sizes = (randomlist k (0, 0.3) seed1)

randomlist n r = take n . unfoldr (Just . randomR r)

main = do
  (progname,_) <- getArgsAndInitialize
  createWindow "Hello World"
  reshapeCallback $= Just reshape
  seed1 <- newStdGen
  seed2 <- newStdGen
  vx <- newIORef 0.0
  vy <- newIORef 0.0
  ax <- newIORef 0.0
  ay <- newIORef 0.0
  shipRotate <- newIORef 90.0
  asteriods <- newIORef $ genAsteriods 50 seed1 seed2
  keyboardMouseCallback $= Just (keyboardMouse shipRotate ax ay)
  displayCallback $= (display vx vy shipRotate asteriods)
  idleCallback $= Just (idle vx vy ax ay)
  mainLoop