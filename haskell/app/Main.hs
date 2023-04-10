module Main where

import System.Environment
import Compiler



main :: IO ()
main = do args <- getArgs
          case args of []  ->  putStrLn "No argument"
                       [s] ->  print(ccomp (read (head args)))
