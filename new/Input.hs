module Input(KeyInfo(..), KeySet, KeySetP, setupInput, isPressed, keyUp, keyDown, keyLeft, keyRight) where

import Data.IORef
import Graphics.UI.GLUT
import Graphics.UI.GLUT.DeviceControl
import Graphics.UI.GLUT.Raw.Callbacks
import Graphics.UI.GLUT.Raw.Functions

import qualified Data.Set as Set

data KeyInfo = BasicKey Char | OtherKey Int
  deriving (Eq, Ord, Show)

type KeySet = Set.Set KeyInfo
type KeySetP = IORef KeySet

setupInput :: IO KeySetP
setupInput = do
  perWindowKeyRepeat $= PerWindowKeyRepeatOff
  keysDown <- newIORef Set.empty
  makeKeyboardFunc (addBasicKey keysDown) >>= glutKeyboardFunc
  makeKeyboardUpFunc (removeBasicKey keysDown) >>= glutKeyboardUpFunc
  makeSpecialFunc (addSpecialKey keysDown) >>= glutSpecialFunc
  makeSpecialUpFunc (removeSpecialKey keysDown) >>= glutSpecialUpFunc
  return keysDown

addBasicKey :: KeySetP -> KeyboardFunc
addBasicKey keySet cuchar _ _ = keySet $~ Set.insert (BasicKey $ toEnum $ fromIntegral cuchar)

removeBasicKey :: KeySetP -> KeyboardUpFunc
removeBasicKey keySet cuchar _ _ = keySet $~ Set.delete (BasicKey $ toEnum $ fromIntegral cuchar)


addSpecialKey :: KeySetP -> SpecialFunc
addSpecialKey keySet i _ _ = keySet $~ Set.insert (OtherKey $ fromIntegral i)

removeSpecialKey :: KeySetP -> SpecialUpFunc
removeSpecialKey keySet i _ _ = keySet $~ Set.delete (OtherKey $ fromIntegral i)

isPressed :: KeySet -> KeyInfo -> Bool
isPressed = flip Set.member

keyUp = OtherKey 101
keyDown = OtherKey 103
keyLeft = OtherKey 100
keyRight = OtherKey 102

