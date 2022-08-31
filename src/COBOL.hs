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

data Prog = Prog IdentDiv 
                 EnvDiv
                 DataDiv 
                 ProcDiv 
          deriving Show

-- Identification Division

data IdentDiv = IdentDiv 
  { identProgId :: T.Text
  , identAuthor :: T.Text
  } deriving Show


-- Environment Division

type EnvDiv = [FileCtrl]
data FileCtrl = FCSelect T.Text T.Text
              deriving Show

-- Data Division

data DataDiv = DataDiv 
  { dataFiles :: [FileDesc]
  , dataStorage :: [Record] 
  } deriving (Show)

instance Semigroup DataDiv where 
  (DataDiv files1 storage1) <> (DataDiv files2 storage2) = 
    DataDiv (files1 <> files2) (storage1 <> storage2)

instance Monoid DataDiv where 
  mempty = DataDiv [] []

data FileDesc = FileDesc T.Text Record
              deriving Show

data Record = RGroup Int T.Text [Record]
            | RElem  Int T.Text RFmt
            deriving Show

data RFmt = RFmt [RFmtChar] RUsage (Maybe Value)
          deriving Show

data RUsage = RDisplay 
            | RComp3
            deriving Show

data RFmtChar = RFAlphaNum  -- X
              | RFNum       -- 9
              | RFSign      -- S
              | RFDec       -- V
              | RFDecPer    -- .
              | RFCurrency  -- $
              | RFComma     -- ,
              deriving Show

-- Procedure Division

type ProcDiv = [Statement]

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
  Prog <$> (lineStartSpace >> identificationDivision) 
       <*> option mempty environmentDivision              
       <*> option mempty dataDivision                     
       <*> procedureDivision                          

identificationDivision :: Parser IdentDiv
identificationDivision = do
  _ <- symbol kIdentification >> symbol kDivision >> period
  IdentDiv <$> programId <*> option "" author
  where 
    programId :: Parser T.Text
    programId = symbol kProgramId >> period >> restOfLine

    author :: Parser T.Text
    author = symbol kAuthor >> period >> restOfLine

environmentDivision :: Parser EnvDiv
environmentDivision = do
  _ <- symbols [kEnvironment, kDivision] >> period
  _ <- symbols [kInputOutput, kSection ] >> period
  _ <- symbol kFileControl >> period
  many (selectStatement <* period)
  where 
    selectStatement :: Parser FileCtrl
    selectStatement = 
      FCSelect <$> (symbol kSelect >> word)
               <*> (symbols [kAssign, kTo] >> word)

dataDivision :: Parser DataDiv
dataDivision = do 
  _ <- symbols [kData, kDivision] >> period
  DataDiv <$> option mempty fileSection 
          <*> option mempty storageSection
  where 
    fileSection :: Parser [FileDesc]
    fileSection = do
      _ <- symbols [kFile, kSection] >> period
      many fileDescriptor
    
    storageSection :: Parser [Record]
    storageSection = do 
      _ <- symbols [kWorkingStorage, kSection] >> period
      many record
    


fileDescriptor :: Parser FileDesc
fileDescriptor = 
  FileDesc <$> header <*> record
  where 
    header :: Parser T.Text
    header = symbol kFd >> word <* (symbols [kRecording, kMode, "F"] >> period)

record :: Parser Record
record = do 
  level <- (decimal :: Parser Int)
  name  <- word
  choice 
    [ RElem  level name <$> (symbol kPic >> recordFormat <* period)
    , RGroup level name <$> (period >> many (recordAbove level))
    ]
  where 
    recordAbove :: Int -> Parser Record
    recordAbove level_limit = do 
      level <- (lookAhead decimal :: Parser Int) 
      if level > level_limit 
        then record
        else fail $ "Expecting record above: " ++ show level_limit

recordFormat :: Parser RFmt
recordFormat =
  RFmt <$> fmtChars <*> fmtUsage <*> fmtInitValue 
  where 
    fmtChars :: Parser [RFmtChar]
    fmtChars = lexeme (concat <$> many fmtCharItem)

    fmtCharItem :: Parser [RFmtChar]
    fmtCharItem = do 
      c <- recordFormatChar 
      option [c] ((`replicate` c) <$> fmtCharItemCount) -- account for replication e.g. X(3)
    
    fmtCharItemCount :: Parser Int
    fmtCharItemCount = between (char '(') (char ')') L.decimal

    fmtUsage :: Parser RUsage
    fmtUsage = option RDisplay (RComp3 <$ symbol kComp3)

    fmtInitValue :: Parser (Maybe Value)
    fmtInitValue = optional (symbol kValue >> value)

recordFormatChar :: Parser RFmtChar
recordFormatChar = choice 
  [ RFAlphaNum <$ char 'X'
  , RFNum      <$ char '9'
  , RFSign     <$ char 'S'
  , RFDec      <$ char 'V'
  , RFDecPer   <$ try (char '.' <* lookAhead recordFormatChar)
  , RFCurrency <$ char '$'
  , RFComma    <$ char ','
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
  [ StrVal     <$> strLit
  , NumVal     <$> decimal
  , VarVal     <$> word
  , NumVal 0   <$  symbol kZero
  , StrVal " " <$  symbol kSpace
  ]

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

symbols :: [T.Text] -> Parser [T.Text]
symbols = traverse symbol

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

kEnvironment :: T.Text
kEnvironment = "ENVIRONMENT"

kInputOutput :: T.Text
kInputOutput = "INPUT-OUTPUT"

kFileControl :: T.Text
kFileControl = "FILE-CONTROL"

kSelect :: T.Text
kSelect = "SELECT"

kAssign :: T.Text
kAssign = "ASSIGN"

kWorkingStorage :: T.Text
kWorkingStorage = "WORKING-STORAGE"

kData :: T.Text
kData           = "DATA"

kFile :: T.Text
kFile           = "FILE"

kFd :: T.Text
kFd             = "FD"

kRecording :: T.Text
kRecording      = "RECORDING"

kMode :: T.Text
kMode           = "MODE"

kComp3 :: T.Text
kComp3          = "COMP-3"

kValue :: T.Text
kValue          = "VALUE"

kSpace :: T.Text
kSpace          = "SPACE"

kZero :: T.Text
kZero           = "ZERO"

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

kGoBack :: T.Text
kGoBack         = "GOBACK"


failT :: MonadFail m => T.Text -> m a
failT = fail . T.unpack
