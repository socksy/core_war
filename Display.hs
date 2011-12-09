module Display where

import Graphics.Gloss
import Graphics.Gloss.Interface.Simulate
import Redcode
import VM

drawSystem :: System -> Picture
drawSystem s =
		let size = coresize;
			core = system_core s;
		in  Pictures [
					  check core 0 0 
					       ]


check :: [Loc] -> Int -> Int -> Picture
check [] c r = Blank							 		          
check (x:xs) c r = 
	let ins = instruction x;
	in	if ins == DAT then Pictures [Color red 
						 	 		 $ Polygon [(fromIntegral (-300+c),fromIntegral (150-r)),
						 			   			(fromIntegral (-300+c+10),fromIntegral (150-r)),
						 			   			(fromIntegral (-300+c+10),fromIntegral (150-r-10)),
						 			   			(fromIntegral (-300+c),fromIntegral (150-r-10))], 
									  if c < 590 then check xs (c+10) r
									  			 else check xs 0 (r+10)]
					  else Pictures [Color green 
						 	 		 $ Polygon [(fromIntegral (-300+c),fromIntegral (150-r)),
						 			   			(fromIntegral (-300+c+10),fromIntegral (150-r)),
						 			   			(fromIntegral (-300+c+10),fromIntegral (150-r-10)),
						 			   			(fromIntegral (-300+c),fromIntegral (150-r-10))], 
									  if c < 590 then check xs (c+10) r
									  			 else check xs 0 (r+10)]


runVM :: System -> IO ()
runVM s = simulateInWindow "Core War" (800, 400) (350, 250) white 1
                           s drawSystem (\_ _ s -> step s)
