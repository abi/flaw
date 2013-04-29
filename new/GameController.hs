module GameController(play) where

import Input
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Callbacks.Global
import Data.IORef
import Control.Monad (when)
import System.Exit


play :: Int -> world -> (world -> IO ()) -> (KeySet -> world -> world) -> (world -> world) -> IO ()
play hertz initWorld displayFunc inputUpdate worldUpdate = do
  let time = 1000 `div` hertz
  keysP <- setupInput
  let gameStep world = do
                        keys <- readIORef keysP
                        when (isPressed keys (BasicKey '\ESC')) exitSuccess
                        let world' = worldUpdate $ inputUpdate keys world
                        displayFunc world'
                        addTimerCallback time (gameStep world')
  gameStep initWorld
