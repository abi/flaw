{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
module GameObjects (makeShip, moveShip, thrustShip, rotateShip, _x, _y, pos, vel, angle, Ship()) where

import Control.Lens
import Data.NumInstances

type Vector = (Double, Double)
data Ship = Ship { _pos :: Vector
                 , _vel :: Vector
                 , _angle :: Double
                 }
  deriving Show
makeLenses ''Ship

_x = _1
_y = _2

makeShip :: Vector -> Ship
makeShip p = Ship p (0,0) 0

moveShip :: Ship -> Ship
moveShip s = s & pos +~ (s^.vel)

thrustShip :: Double -> Ship -> Ship
thrustShip mag s = s & vel +~ ((-mag) * sin (s^.angle.to radians),
                               mag * cos (s^.angle.to radians))

rotateShip :: Double -> Ship -> Ship
rotateShip deg s = s & angle +~ deg

radians :: Floating a => a -> a
radians = (/ 180) . (* pi)
