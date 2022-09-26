{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}

module COBOL.Keyword 
  ( Keyword (..)
  ) where

import GHC.Generics ( Generic(from) )
import Generics.Deriving.Enum
import Generic.GShowEnum ( GShowEnum(gshowEnum) )
import Control.Applicative
import Data.Char
import Data.List (intercalate)
import Data.List.Split
import Text.Read
import Generic.GReadEnum
import Data.Text.Util

data Keyword = KDivision 
             | KIdentification
             | KProgramId
             | KAuthor
             | KEnvironment
             | KInputOutput
             | KFileControl 
             | KSelect 
             | KAssign 
             | KWorkingStorage
             | KData
             | KFile 
             | KFd
             | KRecording
             | KMode
             | KComp3
             | KValue
             | KSpace
             | KSpaces
             | KZero
             | KProcedure
             | KSection 
             | KPic 
             | KOpen
             | KClose
             | KInput
             | KOutput
             | KRead 
             | KAt
             | KEnd
             | KEndRead
             | KWrite
             | KFrom
             | KAfter
             | KAdvancing 
             | KLines
             | KDisplay 
             | KMove 
             | KTo 
             | KFunction
             | KCompute
             | KPerform
             | KUntil
             | KEndPerform
             | KGoback
             deriving (Generic)

instance GEnum Keyword

instance GReadEnum Keyword

instance Show Keyword where 
  show = showKeyword . gshowEnum . from

instance Read Keyword where 
  readPrec :: ReadPrec Keyword
  readPrec = do
    str <- some get
    case greadEnum (keywordToConStr str) of 
      Nothing -> fail $ "Keyword no parse: " ++ str
      Just res -> pure res

keywordToConStr :: String -> String
keywordToConStr str = 
  "K" <> kebab2camel' str

showKeyword :: String -> String
showKeyword = coerceCase . drop 1
  where 
    coerceCase :: String -> String
    coerceCase = intercalate "-" . map (map toUpper) . splitCaps

-- splitCaps "KeyWord" = ["Key", "Word"]
splitCaps :: String -> [String]
splitCaps [] = []
splitCaps (c:cs) = 
  case span isLower cs of 
    (lows, cs') -> (c:lows) : splitCaps cs'

