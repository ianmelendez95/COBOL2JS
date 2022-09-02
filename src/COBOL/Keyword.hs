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
import Data.Char
import Data.List (intercalate)
import Text.Read
import Generic.GReadEnum

import Debug.Trace

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
             | KZero
             | KProcedure
             | KSection 
             | KPic 
             | KDisplay 
             | KMove 
             | KTo 
             | KCompute
             | KGoback
             deriving (Generic)

instance GEnum Keyword

instance GReadEnum Keyword

instance Show Keyword where 
  show = gshowEnum showKeyword . from

instance Read Keyword where 
  readPrec :: ReadPrec Keyword
  readPrec = do
    l <- lexP
    case l of 
      (Ident str) -> 
        case greadEnum str of 
          Nothing -> fail $ "Keyword no parse: " ++ str
          Just res -> pure res
      _ -> fail "Keyword no parse"

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

