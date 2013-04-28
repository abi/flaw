module Bindings (idle,display,reshape,keyboardMouse) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
 
import Display
 
reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)
 
keyboardAct a (SpecialKey KeyLeft) Down = do
  a' <- get a
  a $= a' - 6
keyboardAct a (SpecialKey KeyRight) Down = do
  a' <- get a
  a $= a' + 6
--keyboardAct a p(SpecialKey KeyUp) Down = do
--  (x,y) <- get p
--  p $= (x,y+0.1)
--keyboardAct a p (SpecialKey KeyDown) Down = do
--  (x,y) <- get p
--  p $= (x,y-0.1)
keyboardAct _ _ _ = return ()
 
keyboardMouse angle key state modifiers position = do
  keyboardAct angle key state 