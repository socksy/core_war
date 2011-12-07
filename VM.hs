module VM where

import Redcode
import Control.Monad.State

-- Get the data stored at location i in the core - if the location
-- is out of range, it should wrap around.

readCore :: Int -> Core -> Loc
readCore i c = c !! (i `mod` coresize)

-- Update the core with a new value at location i

writeCore :: Int -> Loc -> Core -> Core
writeCore i = updateIndex (i `mod` coresize)

-- Given a starting location and a field, calculate an absolute
-- memory location

absolute :: Int -> Field -> Core -> Int
absolute l (Val i)  core = l 
absolute l (Addr i) core = l + i
absolute l (AInd i) core = l + i + field_val (a_field (readCore (l + i) core))
absolute l (BInd i) core = l + i + field_val (b_field (readCore (l + i) core))

addField :: Field -> Field -> Field
addField x y = x { field_val = field_val x + field_val y } 

-- Run an instruction at a location in the core.
-- Returns Nothing if the instruction is invalid

step' :: Int -> Loc -> Core -> Maybe (Core, Int) 
step' pc (Loc DAT _ _) _ = Nothing
step' pc (Loc NOP _ _) core = Just (core, pc + 1)
step' pc (Loc ADD (Val x) b) core 
    = let bloc = absolute pc b core
          bl = readCore bloc core in
          Just (writeCore bloc (bl { b_field = addField (b_field bl) (Val x) }) core,
                pc + 1)
step' pc (Loc ADD a b) core
    = let l = readCore (absolute pc a core) core
          bloc = absolute pc b core
          bl = readCore bloc core in
          Just (writeCore bloc (bl { b_field = addField (b_field bl) (a_field l) }) core,
                pc + 1)

-- Call the step' function on each process in the system in turn. If any
-- execute an invalid instruction (i.e. a DAT) remove them from the process
-- list. The winner is the last process running.

step :: System -> System
step (S core ps) = stepAll core [] ps
  where
    stepAll core acc ((name, pc) : ps) 
       = case step' pc (readCore pc core) core of
            Nothing -> -- failed, remove the process
                        stepAll core acc ps
            Just (core', pc') -> stepAll core' 
                                         ((name, pc' `mod` coresize) : acc) ps
    stepAll core acc [] = S core (reverse acc)


