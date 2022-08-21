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

data Prog = Prog Ident Proc deriving Show

newtype Ident = Ident 
  { identProgId :: T.Text
  } deriving Show

newtype Proc = Proc [Statement] deriving Show

newtype Statement = Display T.Text deriving Show

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
  Prog <$> identification  -- identification
       <*> procedure   -- rest of tokens

identification :: Parser Ident
identification = do
  _ <- symbol "IDENTIFICATION"
  _ <- symbol "DIVISION"
  _ <- period
  _ <- symbol "PROGRAM-ID"
  _ <- period
  Ident <$> word <* period

procedure :: Parser Proc
procedure = do
  _ <- symbol "PROCEDURE"
  _ <- symbol "DIVISION"
  _ <- period
  statements <- many statement
  _ <- symbol "GOBACK" >> period
  pure (Proc statements)

statement :: Parser Statement
statement = choice 
  [ displayStatement
  ]

displayStatement :: Parser Statement
displayStatement = do 
  _ <- symbol "DISPLAY"
  Display <$> (strLit <* period)

ctoken :: Parser CToken
ctoken = choice 
  [ Period <$ period
  , StrLit <$> strLit
  , Word <$> word
  ]

word :: Parser T.Text
word = lexeme $ takeWhile1P (Just "<identifier-char>") validIdentifierChar
  where 
    validIdentifierChar :: Char -> Bool
    validIdentifierChar c = isAlphaNum c || c == '-'

period :: Parser ()
period = lexeme . void $ char '.'

strLit :: Parser T.Text
strLit = T.pack <$> lexeme strLit'

strLit' :: Parser String
strLit' = char '"' >> manyTill L.charLiteral (char '"')

symbol :: T.Text -> Parser T.Text
symbol = lexeme . L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

space :: Parser ()
space = L.space space1 empty empty

