module Main where

import Input
import GameObjects
import GameController
import Control.Lens
import Control.Monad.State

import Graphics.Rendering.OpenGL hiding (get)
import Graphics.UI.GLUT hiding (get)

main :: IO ()
main = do
  getArgsAndInitialize
  createWindow "Flaw"
  displayCallback $= return ()
  play
    40
    initWorld
    displayWorld
    inputUpdate
    worldUpdate
  mainLoop


initWorld = makeShip (0,0)

displayWorld s = do
  clear [ColorBuffer]
  preservingMatrix $ do
    centerView 0 0 10
    color $ Color3 1.0 0.0 (1.0 :: GLfloat)
    translate $ Vector3 (realToFrac $ s^.pos^._x) (realToFrac $ s^.pos^._y) (1 :: GLfloat)
    rotate (realToFrac $ s^.angle) $ Vector3 0 0 (1 :: GLfloat)
    renderPrimitive Quads $ vertify3 [(0,0,0), (0,2,0), (1,2,0), (1,0,0)]
  flush

vertify3 :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
vertify3 verts = sequence_ $ map (\(a,b,c) -> vertex $ Vertex3 a b c) verts 

centerView :: GLdouble -> GLdouble -> GLdouble -> IO ()
centerView x y zoom = do
  ortho2D (x - zoom) (x + zoom) (y - zoom) (y + zoom)

inputUpdate :: KeySet -> Ship -> Ship
inputUpdate keys = execState $ do
  s <- get
  when (keys^.contains keyUp) $ modify (thrustShip 0.01)
  when (keys^.contains keyLeft) $ modify (rotateShip 5)
  when (keys^.contains keyRight) $ modify (rotateShip (-5))

worldUpdate :: Ship -> Ship
worldUpdate = moveShip
