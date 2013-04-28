import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
 
import Bindings
 
import Data.List
import System.Random

--- Always keep the z coord 1 since this is a 2D game
genAsteriods :: Int -> StdGen -> StdGen -> [(GLfloat,GLfloat,GLfloat)]
genAsteriods k seed1 seed2 = zip3 xs ys (repeat 1)
                              where xs = (randomlist k (-1,10) seed1)
                                    ys = (randomlist k (-1, 1) seed2)

randomlist n r = take n . unfoldr (Just . randomR r)

main = do
  (progname,_) <- getArgsAndInitialize
  createWindow "Hello World"
  reshapeCallback $= Just reshape
  seed1 <- newStdGen
  seed2 <- newStdGen
  xTranslate <- newIORef 0.0
  shipRotate <- newIORef 0.0
  asteriods <- newIORef $ genAsteriods 30 seed1 seed2
  keyboardMouseCallback $= Just (keyboardMouse shipRotate)
  displayCallback $= (display xTranslate shipRotate asteriods)
  idleCallback $= Just (idle xTranslate)
  mainLoop