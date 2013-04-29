{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color

import qualified Data.Set as S
import Control.Lens
import Control.Monad.State


data World = World { _shipPos :: Point
                   , _shipVel :: Vector
                   , _shipAngle :: Float
                   , _keys :: S.Set Key
                   }
makeLenses ''World

_x = _1
_y = _2

main :: IO ()
main = play
        (InWindow "Flaw" (500, 500) (0,0))
        black
        30
        initWorld
        drawWorld
        handleInput
        stepWorld


initWorld :: World
initWorld = World (0,0) (0,0) 0 S.empty

drawWorld :: World -> Picture
drawWorld w = translate (w^.shipPos._x) (w^.shipPos._y) $
              color white $
              rotate (w^.shipAngle) $
              polygon [(-5, -10), (-5,10), (5,10), (5,-10)]

handleInput :: Event -> World -> World
handleInput (EventKey key state _ _) = keys.contains key .~ (state == Down)
handleInput _                        = id

stepWorld :: Float -> World -> World
stepWorld time = execState $ do
  updateShip time


updateShip :: Float -> State World ()
updateShip time = do
  w <- get
  let keyPressed key = w^.keys.contains (SpecialKey key)
  when (keyPressed KeyUp)   $ shipVel += (20 * sin (w^.shipAngle.to radians), 3 * cos (w^.shipAngle.to radians))
  -- when (keyPressed KeyDown) $ shipVel += (0, (-100))
  when (keyPressed KeyLeft)  $ shipAngle -= 300 * time
  when (keyPressed KeyRight) $ shipAngle += 300 * time
  shipPos += (w^.shipVel & both *~ time)


radians :: Floating a => a -> a
radians = (/ 180) . (* pi)
