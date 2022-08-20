module COBOL where 

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

newtype Prog = Prog String deriving Show

readFile :: FilePath -> IO Prog
readFile = fmap parse . TIO.readFile

parse :: T.Text -> Prog
parse = undefined
