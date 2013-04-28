module GameLoop where

import Input
import Graphics.UI.GLUT.Callbacks.Global
import Data.IORef

registerGameLoop :: Timeout -> TimerCallback -> IO ()
registerGameLoop t fn = addTimerCallback t $ do
                          fn
                          registerGameLoop t fn


makeGameLoop :: KeySetP -> IORef a -> (KeySet -> a -> a) -> IO ()
makeGameLoop keySetP stateP gameFunc = do
  -- TODO: I think these calculations won't actually be done until they're actually used (i.e. when graphics are displayed).  This is probably problematic
  keySet <- readIORef keySetP
  modifyIORef stateP (gameFunc keySet)
