{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
module GameObjects where

import Control.Lens
import Data.NumInstances
import Graphics.Rendering.OpenGL(Color3, GLfloat)

type Vector = (Double, Double)
data Phys = Phys { _pos :: Vector
                 , _vel :: Vector
                 }
  deriving Show
makeClassy ''Phys

data Ship = Ship { _shipPhys :: Phys
                 , _angle :: Double
                 }
  deriving Show
makeLenses ''Ship

instance HasPhys Ship where
  phys = shipPhys

_x = _1
_y = _2

makeShip :: Vector -> Ship
makeShip p = Ship (Phys p (0,0)) 0

moveShip :: Ship -> Ship
moveShip s = s & pos +~ (s^.vel)

thrustShip :: Double -> Ship -> Ship
thrustShip mag s = s & vel +~ ((-mag) * sin (s^.angle.to radians),
                               mag * cos (s^.angle.to radians))

rotateShip :: Double -> Ship -> Ship
rotateShip deg s = s & angle +~ deg

radians :: Floating a => a -> a
radians = (/ 180) . (* pi)


data Asteroid = Asteroid { _astPhys :: Phys
                         , _size :: Double
                         , _clr :: Color3 GLfloat
                         }
  deriving Show
makeLenses ''Asteroid

instance HasPhys Asteroid where
  phys = astPhys

makeAsteroid :: Vector -> Vector -> Double -> Color3 GLfloat -> Asteroid
makeAsteroid p v s c = Asteroid (Phys p v) s c


data World = World { _ship :: Ship
                   , _asteroids :: [Asteroid]
                   }
  deriving Show
makeLenses ''World
