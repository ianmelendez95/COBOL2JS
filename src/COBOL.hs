module COBOL where 

import Data.Void
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

newtype Prog = Prog [CToken] deriving Show

data CToken = Word T.Text
            | StrLit T.Text
            | Period
            deriving Show

readFile :: FilePath -> IO Prog
readFile file_path = do 
  content <- TIO.readFile file_path
  let result = parse prog file_path content
  case result of 
    Left err -> error . errorBundlePretty $ err
    Right res -> pure res

prog :: Parser Prog
prog = undefined

ctoken :: Parser CToken
ctoken = choice 
  [ period
  , strLit
  , word
  ]

word :: Parser CToken
word = Word <$> takeWhile1P (Just "Identifier Char") validIdentifierChar
  where 
    validIdentifierChar :: Char -> Bool
    validIdentifierChar c = isDigit c || c == '-'

period :: Parser CToken
period = Period <$ char '.'

strLit :: Parser CToken
strLit = StrLit . T.pack <$> strLit'

strLit' :: Parser String
strLit' = char '"' >> manyTill L.charLiteral (char '"')

space :: Parser ()
space = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

