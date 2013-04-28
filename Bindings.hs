module Bindings (idle,display,reshape,keyboardMouse) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
 
import Display

-- pi = 3.14
radians a = (a / 180) * pi
 
reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)
 
keyboardAct a ax ay (SpecialKey KeyLeft) Down = do
  a' <- get a
  a $= a' + 6
keyboardAct a ax ay (SpecialKey KeyRight) Down = do
  a' <- get a
  --print a'
  a $= a' - 6
-- TODO: Is angle in radians?
keyboardAct a ax ay (SpecialKey KeyUp) Down = do
  ax' <- get ax
  ay' <- get ay
  a' <- get a
  --print ax'
  --print ay'
  ax $= (ax' + (cos $ radians a') * 0.00001)
  ay $= (ay' + (sin $ radians a') * 0.00001)
--keyboardAct a ax ay p(SpecialKey KeyUp) Down = do
--  ax' <- get ax
--  ax $= ax' + 1
--keyboardAct a p (SpecialKey KeyDown) Down = do
--  (x,y) <- get p
--  p $= (x,y-0.1)
keyboardAct _ _ _ _ _ = return ()
 
keyboardMouse angle ax ay key state modifiers position = do
  keyboardAct angle ax ay key state 