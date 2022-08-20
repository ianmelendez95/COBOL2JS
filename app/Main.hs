module Main (main) where

import Lib
import System.Environment
import COBOL

main :: IO ()
main = do
  [file] <- getArgs
  prog <- COBOL.readFile file
  print prog
