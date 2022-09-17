module Main (main) where

import System.Environment
import qualified COBOL.Syntax as COBOL

main :: IO ()
main = do
  [file] <- getArgs
  prog <- COBOL.readFile file
  print prog
