module Main where

import Input
import GameObjects
import Display
import GameController
import Control.Lens
import Control.Monad.State
import Graphics.Rendering.OpenGL(Color3(..))

import Graphics.UI.GLUT (mainLoop)
main :: IO ()
main = do
  initGraphics
  world <- initWorld
  play
    40
    world
    displayWorld
    inputUpdate
    worldUpdate
  mainLoop


initWorld :: IO World
initWorld = do 
  ast <- replicateM 100 (randomAsteroid (-100,100) (-1,1) (1,5))
  return $ World (makeShip (0,0)) ast

inputUpdate :: KeySet -> World -> World
inputUpdate keys = execState $ do
  when (keys^.contains keyUp) $ ship %= thrustShip 0.1
  when (keys^.contains keyLeft) $ ship %= rotateShip 7
  when (keys^.contains keyRight) $ ship %= rotateShip (-7)

worldUpdate :: World -> World
worldUpdate = execState $ do
  ship %= movePhys
  asteroids.mapped %= movePhys
  -- col <- collisionDetected
