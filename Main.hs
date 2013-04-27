import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
 
import Bindings
 
main = do
  (progname,_) <- getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= display
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  mainLoop