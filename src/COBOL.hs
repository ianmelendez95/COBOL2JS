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

data Prog = Prog IdentDiv DataDiv ProcDiv deriving Show

data Statement = Display [Value]
               | Move Value T.Text
               | Compute T.Text Arith
               deriving Show
              
data Arith = AVal Value
           | Mult Arith Arith
           deriving Show

type IdentDiv = T.Text
type DataDiv = [Var]
type ProcDiv = [Statement]

data Var = Var T.Text VType Int deriving Show
data VType = AlphaNum | Num deriving Show

data Value = VarVal T.Text
           | NumVal Int
           | StrVal T.Text
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
  Prog <$> (space >> identification)  -- identification
       <*> (dataDivision <|> pure []) -- optional data
       <*> procedureDivision          -- procedure

identification :: Parser IdentDiv
identification = do
  _ <- symbol "IDENTIFICATION" >> symbol "DIVISION" >> period
  _ <- symbol "PROGRAM-ID" >> period
  word <* period

dataDivision :: Parser DataDiv
dataDivision = do 
  _ <- symbol "DATA" >> symbol "DIVISION" >> period
  _ <- symbol "WORKING-STORAGE" >> symbol "SECTION" >> period
  many variable

variable :: Parser Var
variable =
  Var <$> (symbol "77" >> word)
      <*> (symbol "PIC" >> variableType)
      <*> (between (char '(') (char ')') L.decimal <* period)

variableType :: Parser VType
variableType = choice 
  [ AlphaNum <$ char 'X'
  , Num <$ char '9'
  ]

procedureDivision :: Parser ProcDiv
procedureDivision = do
  _ <- symbol "PROCEDURE" >> symbol "DIVISION" >> period
  many statement <* (symbol "GOBACK" >> period)

statement :: Parser Statement
statement = choice 
  [ displayStatement
  , moveStatement
  , computeStatement
  ]

displayStatement :: Parser Statement
displayStatement = Display <$> (symbol "DISPLAY" >> some value <* period)

moveStatement :: Parser Statement 
moveStatement =
  Move <$> (symbol "MOVE" >> value)
       <*> (symbol "TO" >> word <* period)

computeStatement :: Parser Statement
computeStatement = 
  Compute <$> (symbol "COMPUTE" >> word)
          <*> (symbol "=" >> arithmeticExpression)

arithmeticExpression :: Parser Arith
arithmeticExpression = 
  Mult <$> (AVal <$> value)
       <*> (symbol "*" >> (AVal <$> value) <* period)

value :: Parser Value
value = choice [StrVal <$> strLit, NumVal <$> decimal, VarVal <$> word]

word :: Parser T.Text
word = lexeme $ T.cons <$> first <*> rest
  where 
    first :: Parser Char
    first = satisfy isAlpha

    rest :: Parser T.Text
    rest = takeWhile1P (Just "<identifier-char>") validIdentifierChar

    validIdentifierChar :: Char -> Bool
    validIdentifierChar c = isAlphaNum c || c == '-'

period :: Parser ()
period = lexeme . void $ char '.'

decimal :: Num a => Parser a
decimal = lexeme L.decimal

strLit :: Parser T.Text
strLit = T.pack <$> lexeme strLit'

strLit' :: Parser String
strLit' = char '"' >> manyTill L.charLiteral (char '"')

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

space :: Parser ()
space = L.space space1 empty empty

