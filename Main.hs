module Main where

import Redcode
import VM
import Display

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
main = runVM test_state

