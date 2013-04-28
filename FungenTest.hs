module Main where

import Graphics.UI.Fungen
import Graphics.UI.Fungen

data GameAttribute = Score Int

type ShipObject = GameObject ()

type FlawAction a = IOGame GameAttribute () () () a

main :: IO ()
main = do
  let winConfig = ((0,0), (500,500), "Hello world!")
      ship = objectGroup "shipGroup" (replicate 10 createShip)
      initScore = Score 0
      gameMap = colorMap 0.0 0.0 0.0 1.0 0.0
      input = [(SpecialKey KeyRight, StillDown, turnShip 1),
               (SpecialKey KeyLeft,  StillDown, turnShip (-1)),
               (SpecialKey KeyUp,    StillDown, thrustShip 1),
               (SpecialKey KeyDown,    StillDown, thrustShip (-1))
               ]
               -- (SpecialKey KeyDown, Press, \_ _ -> funExit)]
  funInit winConfig gameMap [ship] () initScore input gameCycle (Timer 20) []


createShip :: ShipObject
createShip = let shipPic = Basic (Polyg [(0,0), (0,10), (10,10), (10,0)] 1.0 0.0 0.0 Filled)
             in object "ship" shipPic False (125, 125) (0,0) ()


turnShip i _ _ = do
  ship <- findObject "ship" "shipGroup"
  (pX, pY) <- getObjectSpeed ship
  setObjectSpeed (pX + fromIntegral i, pY) ship

thrustShip i _ _ = do
  ship <- findObject "ship" "shipGroup"
  (pX, pY) <- getObjectSpeed ship
  setObjectSpeed (pX, pY + i) ship


gameCycle :: FlawAction ()
gameCycle = do
  (Score n) <- getGameAttribute
  ship <- findObject "ship" "shipGroup"
  spd <- calculateVelocityMagnitude ship
  printOnScreen (show spd) TimesRoman24 (0,0) 1.0 1.0 1.0
  showFPS TimesRoman24 (30,0) 1.0 0.0 0.0


calculateVelocityMagnitude :: ShipObject -> FlawAction Double
calculateVelocityMagnitude obj = do
  (vx, vy) <- getObjectSpeed obj
  return $ realToFrac $ (vx ** 2 + vy ** 2) ** 0.5
