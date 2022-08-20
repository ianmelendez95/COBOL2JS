module COBOL where 

import Data.Void
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

newtype Prog = Prog [String] deriving Show

readFile :: FilePath -> IO Prog
readFile file_path = do 
  content <- TIO.readFile file_path
  let result = parse prog file_path content
  case result of 
    Left err -> error . errorBundlePretty $ err
    Right res -> pure res

prog :: Parser Prog
prog = undefined

space :: Parser ()
space = L.space space1 empty empty

