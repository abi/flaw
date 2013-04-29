module Bindings (idle,display,reshape,keyboardMouse) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
 
import Display

-- pi = 3.14
 
reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)
 
keyboardAct a acc (SpecialKey KeyLeft) Down = do
  a' <- get a
  a $= a' + 6
keyboardAct a acc (SpecialKey KeyRight) Down = do
  a' <- get a
  --print a'
  a $= a' - 6
-- TODO: Is angle in radians?
keyboardAct a acc (SpecialKey KeyUp) Down = do
  acc $= True

keyboardAct a acc (SpecialKey KeyUp) Up = do
  acc $= False

--keyboardAct a acc p(SpecialKey KeyUp) Down = do
--  ax' <- get ax
--  ax $= ax' + 1
--keyboardAct a p (SpecialKey KeyDown) Down = do
--  (x,y) <- get p
--  p $= (x,y-0.1)
keyboardAct _ _ _ _ = return ()
 
keyboardMouse angle acc key state modifiers position = do
  keyboardAct angle acc key state 