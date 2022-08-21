{-# LANGUAGE OverloadedStrings #-}


module COBOL where 

import Control.Monad (void)
import Data.Void
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

data Prog = Prog Ident [CToken] deriving Show

data Ident = Ident 
  { identProgId :: T.Text
  } deriving Show

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
prog = do 
  _ <- space -- skip initial whitespace
  Prog <$> lexeme identification  -- identification
       <*> some (lexeme ctoken)   -- rest of tokens

identification :: Parser Ident
identification = do
  _ <- lexeme (symbol "IDENTIFICATION")
  _ <- lexeme (symbol "DIVISION")
  _ <- lexeme period
  _ <- lexeme (symbol "PROGRAM-ID")
  _ <- lexeme period
  Ident <$> lexeme word <* period

ctoken :: Parser CToken
ctoken = choice 
  [ Period <$ period
  , StrLit <$> strLit
  , Word <$> word
  ]

word :: Parser T.Text
word = takeWhile1P (Just "<identifier-char>") validIdentifierChar
  where 
    validIdentifierChar :: Char -> Bool
    validIdentifierChar c = isAlphaNum c || c == '-'

period :: Parser ()
period = void $ char '.'

strLit :: Parser T.Text
strLit = T.pack <$> strLit'

strLit' :: Parser String
strLit' = char '"' >> manyTill L.charLiteral (char '"')

space :: Parser ()
space = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

