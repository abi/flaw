import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
 
import Bindings
 
main = do
  (progname,_) <- getArgsAndInitialize
  createWindow "Hello World"
  reshapeCallback $= Just reshape
  xTranslate <- newIORef 0.0
  shipRotate <- newIORef 0.0
  keyboardMouseCallback $= Just (keyboardMouse shipRotate)
  displayCallback $= (display xTranslate shipRotate)
  idleCallback $= Just (idle xTranslate)
  mainLoop