module Main where

import Redcode
import VM
import Display
import System
import Grammar

-- Simple version of the main program: set up a test state, wait for
-- user input and execute one step, until only one process left
{-
main :: IO ()
main = loop test_state 0

loop :: System -> Int -> IO ()
loop s i = do print s
              x <- getLine
              let s' = step s
              case processes s' of
                  [(n,_)] -> putStrLn $ n ++ " wins after " ++ show i ++ " iterations"
                  _ -> loop s' (i + 1)

-- Replace with the following for the graphical version
-}

main :: IO () 
main = do file_list <- getArgs
          file_1 <- readFile $ head file_list
          file_2 <- readFile $ last file_list
          let war_1 = Warrior "war1" (eval file_1);
          	  war_2 = Warrior "war2" (eval file_2);
          	  state = addWarrior war_2 (addWarrior war_1 init_state 0) 1;
          runVM state

