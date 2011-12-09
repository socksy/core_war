module Display where

import Graphics.Gloss
import Graphics.Gloss.Interface.Simulate
import Redcode
import VM

drawSystem :: System -> Picture
drawSystem s =
		let size = coresize;
			core = system_core s;
			ps = processes s;
			a = map (\x -> x ++ "   ") [name ++ " running on PC: " ++ show pc | (name,pc) <- ps];
		in  Pictures [Color yellow
					$ Translate (-130) (250)
					$ Scale 0.4 0.4
					$ Text "Core War",
					  Color red
					$ Translate (-350) (-280)
					$ Scale 0.2 0.2
					$ Text (unlines a),
					  check core ps 0 0 0					   
					       					]

check :: [Loc] -> [(String, PC)] -> Int -> Int -> Int -> Picture
check [] ps c r i = Blank							 		          
check (x:xs) ps c r i = 
	let ins = instruction x;
	in Pictures [if ins == DAT then Color red $ draw c r
							   else if any (\x -> snd(x) == i) ps then Color white $ draw c r 
							   									  else Color green $ draw c r 
				 ,if c < 590 then check xs ps (c+10) r (i+1)
				   			 else check xs ps 0 (r+10) (i+1)]

draw :: Int -> Int -> Picture
draw c r = Polygon [(fromIntegral (-300+c),fromIntegral (150-r)),
		   			(fromIntegral (-300+c+10),fromIntegral (150-r)),
		   			(fromIntegral (-300+c+10),fromIntegral (150-r-10)),
		   			(fromIntegral (-300+c),fromIntegral (150-r-10))]

runVM :: System -> IO ()
runVM s = simulateInWindow "Core War" (800, 600) (500, 200) black 1
                           s drawSystem (\_ _ s -> step s)

