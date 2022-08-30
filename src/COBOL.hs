{-# LANGUAGE OverloadedStrings #-}

module COBOL 
  ( Prog (..)
  , Statement (..)
  , Value (..)

  , Arith (..)
  , ABin (..)
  , AVal (..)
  , AOp (..)

  , COBOL.readFile
  , arithValToValue
  ) where 

import Control.Monad (void)
import Data.Void
import Data.Char
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Foldable as Set

type Parser = Parsec Void T.Text

data Prog = Prog IdentDiv DataDiv ProcDiv deriving Show

data Statement = Display [Value]
               | Move Value T.Text
               | Compute T.Text Arith
               | GoBack
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

data IdentDiv = IdentDiv 
  { identProgId :: T.Text
  , identAuthor :: T.Text
  } deriving Show

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
  Prog <$> (lineStartSpace >> identification)  -- identification
       <*> (dataDivision <|> pure []) -- optional data
       <*> procedureDivision          -- procedure

identification :: Parser IdentDiv
identification = do
  _ <- symbol kIdentification >> symbol kDivision >> period
  IdentDiv <$> programId <*> option "" author
  where 
    programId :: Parser T.Text
    programId = symbol kProgramId >> period >> restOfLine

    author :: Parser T.Text
    author = symbol kAuthor >> period >> restOfLine

dataDivision :: Parser DataDiv
dataDivision = do 
  _ <- symbol kData >> symbol kDivision >> period
  _ <- symbol kWorkingStorage >> symbol kSection >> period
  many (variable <* period)

variable :: Parser Var
variable =
  Var <$> (symbol "77" >> word)
      <*> (symbol kPic >> variableType)
      <*> between (char '(') (char ')') L.decimal

variableType :: Parser VType
variableType = choice 
  [ AlphaNum <$ char 'X'
  , Num <$ char '9'
  ]

procedureDivision :: Parser ProcDiv
procedureDivision = do
  _ <- symbol kProcedure >> symbol kDivision >> period
  concat <$> many sentence

sentence :: Parser [Statement]
sentence = many statement <* period

statement :: Parser Statement
statement = choice 
  [ displayStatement
  , moveStatement
  , computeStatement
  , GoBack <$ symbol kGoBack
  ]

displayStatement :: Parser Statement
displayStatement = Display <$> (symbol kDisplay >> some value)

moveStatement :: Parser Statement 
moveStatement =
  Move <$> (symbol kMove >> value)
       <*> (symbol kTo >> word)

computeStatement :: Parser Statement
computeStatement = 
  Compute <$> (symbol kCompute >> word)
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
      [ ANum 0 <$  symbol kZero 
      , ANum   <$> decimal
      , AVar   <$> word
      ]
    
    arithOp :: Parser AOp
    arithOp = choice 
      [ Mult <$ symbol "*"
      , Add  <$ symbol "+"
      ]

value :: Parser Value
value = choice 
  [ StrVal <$> strLit
  , NumVal <$> decimal
  , VarVal <$> word
  ]

-- isKeyword :: T.Text -> Bool
-- isKeyword = 

word :: Parser T.Text
word = try . lexeme $ do
  w <- rawWord
  if isKeyword w 
    then failT ("Expecting word, found keyword: " <> w) 
    else pure w
  where 
    rawWord :: Parser T.Text
    rawWord = T.cons <$> first <*> rest

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
symbol = L.symbol tokenSpace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme tokenSpace

-- Space after tokens (essentially all space except comments, since those only exist on 'empty' lines)
tokenSpace :: Parser ()
tokenSpace = hspace >> option () (newline >> lineStartSpace)

lineStartSpace :: Parser ()
lineStartSpace = hspace >> option () (choice 
  [ newline >> lineStartSpace
  , char '*' >> void restOfLine
  ])

restOfLine :: Parser T.Text
restOfLine = takeWhileP (Just "character") (/= '\n') <* lineStartSpace

-- Keywords

isKeyword :: T.Text -> Bool
isKeyword t = t `Set.elem` keywords

keywords :: Set.Set T.Text
keywords = Set.fromList 
  [ kIdentification
  , kDivision
  , kProgramId
  , kAuthor
  , kWorkingStorage
  , kData
  , kProcedure
  , kSection
  , kPic
  , kDisplay
  , kMove
  , kTo
  , kCompute
  , kZero
  , kGoBack
  ]

kDivision :: T.Text
kDivision       = "DIVISION"

kIdentification :: T.Text
kIdentification = "IDENTIFICATION"

kProgramId :: T.Text
kProgramId      = "PROGRAM-ID"

kAuthor :: T.Text
kAuthor = "AUTHOR"

kWorkingStorage :: T.Text
kWorkingStorage = "WORKING-STORAGE"

kData :: T.Text
kData           = "DATA"

kProcedure :: T.Text
kProcedure      = "PROCEDURE"

kSection :: T.Text
kSection        = "SECTION"

kPic :: T.Text
kPic            = "PIC"

kDisplay :: T.Text
kDisplay        = "DISPLAY"

kMove :: T.Text
kMove           = "MOVE"

kTo :: T.Text
kTo             = "TO"

kCompute :: T.Text
kCompute        = "COMPUTE"

kZero :: T.Text
kZero           = "ZERO"

kGoBack :: T.Text
kGoBack         = "GOBACK"


failT :: MonadFail m => T.Text -> m a
failT = fail . T.unpack
