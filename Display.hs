module Display where

import Graphics.Gloss
import Graphics.Gloss.Interface.Simulate
import Redcode
import VM

drawSystem :: System -> Picture
drawSystem s = Blank

runVM :: System -> IO ()
runVM s = simulateInWindow "Core War" (640, 480) (50, 50) black 1
                           s drawSystem (\_ _ s -> step s)
