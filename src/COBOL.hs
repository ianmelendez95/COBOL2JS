{-# LANGUAGE OverloadedStrings #-}

module COBOL where 

import Control.Monad (void)
import Data.Void
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Byte.Lexer (skipLineComment)

type Parser = Parsec Void T.Text

data Prog = Prog IdentDiv DataDiv ProcDiv deriving Show

data Statement = Display [Value]
               | Move Value T.Text
               | Compute T.Text Arith
               deriving Show
              
data Arith = AVal Value
           | ABin1 ABin
           | ABin2 ABin AOp ABin 
           deriving Show

data ABin = ABin AVal AOp AVal 
          deriving Show

data AOp = Mult 
         | Add
         deriving Show

data AVal = AVar T.Text
          | ANum Int
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

arithValToValue :: AVal -> Value
arithValToValue (AVar v) = VarVal v
arithValToValue (ANum n) = NumVal n

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
  many (variable <* period)

variable :: Parser Var
variable =
  Var <$> (symbol "77" >> word)
      <*> (symbol "PIC" >> variableType)
      <*> between (char '(') (char ')') L.decimal

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
  ] <* period

displayStatement :: Parser Statement
displayStatement = Display <$> (symbol "DISPLAY" >> some value)

moveStatement :: Parser Statement 
moveStatement =
  Move <$> (symbol "MOVE" >> value)
       <*> (symbol "TO" >> word)

computeStatement :: Parser Statement
computeStatement = 
  Compute <$> (symbol "COMPUTE" >> word)
          <*> (symbol "=" >> arithmeticExpression)

-- https://www.ibm.com/docs/en/cobol-zos/6.4?topic=structure-arithmetic-expressions
arithmeticExpression :: Parser Arith
arithmeticExpression = choice
  [ between (symbol "(") (symbol ")") arithBinMaybe2
  , arithBinMaybe2
  ]
  where 
    arithBinMaybe2 :: Parser Arith
    arithBinMaybe2 = do 
      a1 <- arithBin
      (ABin2 a1 <$> arithOp <*> arithBin) <|> pure (ABin1 a1)
    
    arithBin :: Parser ABin
    arithBin = 
      ABin <$> arithVal 
           <*> arithOp
           <*> arithVal

    arithVal :: Parser AVal
    arithVal = choice 
      [ ANum 0 <$  symbol "ZERO" 
      , ANum   <$> decimal
      , AVar   <$> word
      ]
    
    arithOp :: Parser AOp
    arithOp = choice 
      [ Mult <$ symbol "*"
      , Add  <$ symbol "+"
      ]

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
space = L.space space1 (L.skipLineComment "*") (L.skipBlockComment "/*" "*/")

