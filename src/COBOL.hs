{-# LANGUAGE OverloadedStrings #-}

module COBOL 
  ( Prog (..)
  , Para (..)
  , Sentence
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
import Data.Maybe
import Text.Read (readMaybe)
import Data.List ( group )
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec
import Text.Megaparsec.Char ( char, hspace, newline )
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Foldable as Set

import COBOL.Keyword

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

data RUsage = RDisplay 
            | RComp3

data RFmtChar = RFAlphaNum  -- X
              | RFNum       -- 9
              | RFSign      -- S
              | RFDec       -- V
              | RFDecPer    -- .
              | RFCurrency  -- $
              | RFComma     -- ,
              deriving Eq

-- Procedure Division

type ProcDiv = [Para]

data Para = Para (Maybe T.Text) [Sentence]
          deriving Show

type Sentence = [Statement]

data Statement = Display [Value]
               | Move Value T.Text
               | Compute T.Text Arith
               | Open Put T.Text
               | Close T.Text
               | Read T.Text
               | Write T.Text
               | Perform T.Text
               | PerformUntil Cond Sentence
               | GoBack
               deriving Show

data Put = Input | Output 
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

data Cond = Cond CVal COp CVal
          deriving Show

data COp = CEq
         deriving Show

data CVal = CVar T.Text 
          | CStr T.Text
          deriving Show

data Value = VarVal T.Text
           | NumVal Int
           | StrVal T.Text

-- Show Instances

instance Show RFmt where 
  show (RFmt chars usage fmt_val) = unwords 
    [ show KPic
    , showFmtChars chars 
    , showUsage usage
    , maybe "" (\v -> unwords [show KValue, show v]) fmt_val
    ]
    where 
      showUsage :: RUsage -> String
      showUsage RDisplay = ""
      showUsage u = show u

      showFmtChars :: [RFmtChar] -> String
      showFmtChars cs = concatMap showCharGroup $ group cs

      showCharGroup :: [RFmtChar] -> String
      showCharGroup [] = ""
      showCharGroup g@(c:_) = show c ++ "(" ++ show (length g) ++ ")"

instance Show RUsage where 
  show RDisplay = show KDisplay
  show RComp3   = show KComp3

instance Show RFmtChar where 
  show RFAlphaNum = "X"
  show RFNum      = "9"
  show RFSign     = "S"
  show RFDec      = "V"
  show RFDecPer   = "."
  show RFCurrency = "$"
  show RFComma    = ","

instance Show Value where 
  show (VarVal t) = T.unpack t
  show (NumVal n) = show n
  show (StrVal s) = "\"" ++ T.unpack s ++ "\""

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
       <*> (procedureDivision <* eof)

identificationDivision :: Parser IdentDiv
identificationDivision = do
  _ <- keyword KIdentification >> keyword KDivision >> period
  IdentDiv <$> programId <*> option "" author
  where 
    programId :: Parser T.Text
    programId = keyword KProgramId >> period >> restOfLine

    author :: Parser T.Text
    author = keyword KAuthor >> period >> restOfLine

environmentDivision :: Parser EnvDiv
environmentDivision = do
  _ <- keywords [KEnvironment, KDivision] >> period
  _ <- keywords [KInputOutput, KSection ] >> period
  _ <- keyword KFileControl >> period
  many (selectStatement <* period)
  where 
    selectStatement :: Parser FileCtrl
    selectStatement = 
      FCSelect <$> (keyword KSelect >> identifier)
               <*> (keywords [KAssign, KTo] >> identifier)

dataDivision :: Parser DataDiv
dataDivision = do 
  _ <- keywords [KData, KDivision] >> period
  DataDiv <$> option mempty fileSection 
          <*> option mempty storageSection
  where 
    fileSection :: Parser [FileDesc]
    fileSection = do
      _ <- keywords [KFile, KSection] >> period
      many fileDescriptor
    
    storageSection :: Parser [Record]
    storageSection = do 
      _ <- keywords [KWorkingStorage, KSection] >> period
      many record

fileDescriptor :: Parser FileDesc
fileDescriptor = 
  FileDesc <$> header <*> record
  where 
    header :: Parser T.Text
    header = keyword KFd >> identifier <* (symbols [showT KRecording, showT KMode, "F"] >> period)

record :: Parser Record
record = do 
  level <- (decimal :: Parser Int)
  name  <- identifier
  choice 
    [ RElem  level name <$> (keyword KPic >> recordFormat <* period)
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
    fmtUsage = option RDisplay (RComp3 <$ keyword KComp3)

    fmtInitValue :: Parser (Maybe Value)
    fmtInitValue = optional (keyword KValue >> value)

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
  _ <- keyword KProcedure >> keyword KDivision >> period
  many paragraph

paragraph :: Parser Para
paragraph = choice 
  [ Para . Just <$> (identifier <* period) <*> many sentence
  , Para Nothing                     <$> many sentence
  ]

sentence :: Parser [Statement]
sentence = many statement <* period

statement :: Parser Statement
statement = choice 
  [ displayStatement
  , moveStatement
  , computeStatement
  , GoBack <$ keyword KGoback
  , openStatement
  , closeStatement
  , readStatement
  , writeStatement
  , performStatement
  ]

displayStatement :: Parser Statement
displayStatement = Display <$> (keyword KDisplay >> some value)

moveStatement :: Parser Statement 
moveStatement =
  Move <$> (keyword KMove >> value)
       <*> (keyword KTo >> identifier)

computeStatement :: Parser Statement
computeStatement = 
  Compute <$> (keyword KCompute >> identifier)
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
      [ ANum 0 <$  keyword KZero 
      , ANum   <$> decimal
      , AVar   <$> identifier
      ]
    
    arithOp :: Parser AOp
    arithOp = choice 
      [ Mult <$ symbol "*"
      , Add  <$ symbol "+"
      ]

openStatement :: Parser Statement
openStatement = 
  Open <$> (keyword KOpen >> choice 
             [ Input  <$ keyword KInput
             , Output <$ keyword KOutput
             ])
       <*> identifier

closeStatement :: Parser Statement
closeStatement = Close <$> (keyword KClose >> identifier)

readStatement :: Parser Statement 
readStatement = Read <$> (keyword KRead >> word)

writeStatement :: Parser Statement 
writeStatement = Write <$> (keyword KWrite >> word)

performStatement :: Parser Statement
performStatement = do
  _ <- keyword KPerform
  untilPhrase <|> simple
  where 
    untilPhrase :: Parser Statement
    untilPhrase = PerformUntil <$> (keyword KUntil >> condition)
                               <*> (many statement <* keyword KEndPerform)
    
    simple :: Parser Statement 
    simple = Perform <$> identifier

condition :: Parser Cond
condition = Cond <$> condValue <*> condOp <*> condValue

condOp :: Parser COp
condOp = CEq <$ symbol "="

condValue :: Parser CVal
condValue = choice 
  [ CStr <$> strLit
  , CVar <$> identifier
  ]

value :: Parser Value
value = choice 
  [ StrVal     <$> strLit
  , NumVal     <$> decimal
  , VarVal     <$> identifier
  , NumVal 0   <$  keyword KZero
  , StrVal " " <$  keyword KSpace
  ]

keywords :: [Keyword] -> Parser [Keyword]
keywords = traverse keyword

keyword :: Keyword -> Parser Keyword
keyword k = try . lexeme $ (k <$ keywordText) 
  where 
    keywordText :: Parser T.Text
    keywordText = chunk (showT k) <* notFollowedBy (satisfy isIdentifierChar)

identifier :: Parser T.Text
identifier = try $ do
  w <- word
  if isKeyword w 
    then failT ("Expecting identifier, found keyword: " <> w) 
    else pure w

word :: Parser T.Text
word = try . lexeme $ (T.cons <$> first <*> rest) 
  where 
    first :: Parser Char
    first = satisfy isAlpha

    rest :: Parser T.Text
    rest = takeWhile1P (Just "<identifier-char>") isIdentifierChar

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '-' 

period :: Parser ()
period = lexeme . void $ char '.'

decimal :: Num a => Parser a
decimal = lexeme L.decimal

strLit :: Parser T.Text
strLit = T.pack <$> lexeme strLit'

strLit' :: Parser String
strLit' = choice 
  [ char '"' >> manyTill L.charLiteral (char '"')
  , char '\'' >> manyTill L.charLiteral (char '\'') 
  ]

symbolsS :: Show a => [a] -> Parser [T.Text]
symbolsS = traverse symbolS

symbols :: [T.Text] -> Parser [T.Text]
symbols = traverse symbol

symbolS :: Show a => a -> Parser T.Text
symbolS = symbol . showT 

symbol :: T.Text -> Parser T.Text
symbol = L.symbol tokenSpace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme tokenSpace

-- Space after toKens (essentially all space except comments, since those only exist on 'empty' lines)
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
isKeyword = isJust . readKeyword . T.unpack
  where 
    readKeyword :: String -> Maybe Keyword
    readKeyword = readMaybe

showT :: Show a => a -> T.Text
showT = T.pack . show

failT :: MonadFail m => T.Text -> m a
failT = fail . T.unpack
