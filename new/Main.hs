module Main where

import Input
import GameObjects
import Display
import GameController
import Control.Lens
import Control.Monad.State

import Graphics.UI.GLUT (mainLoop)
main :: IO ()
main = do
  initGraphics
  play
    40
    initWorld
    displayWorld
    inputUpdate
    worldUpdate
  mainLoop


initWorld = World (makeShip (0,0)) []

inputUpdate :: KeySet -> World -> World
inputUpdate keys = execState $ do
  when (keys^.contains keyUp) $ ship %= thrustShip 0.1
  when (keys^.contains keyLeft) $ ship %= rotateShip 7
  when (keys^.contains keyRight) $ ship %= rotateShip (-7)

worldUpdate :: World -> World
worldUpdate = ship %~ moveShip
