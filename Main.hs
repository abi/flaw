import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
 
import Bindings
 
main = do
  (progname,_) <- getArgsAndInitialize
  createWindow "Hello World"
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  xTranslate <- newIORef 0.0
  displayCallback $= (display xTranslate)
  idleCallback $= Just (idle xTranslate)
  mainLoop