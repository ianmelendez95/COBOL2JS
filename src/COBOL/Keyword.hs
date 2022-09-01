{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module COBOL.Keyword 
  ( Keyword (..)
  ) where

import GHC.Generics ( Generic(from) )
import Generic.GShowEnum ( GShowEnum(gshowEnum) )
import Data.Char
import Data.List (intercalate)

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
             deriving (Ord, Eq, Generic)

instance Show Keyword where 
  show = gshowEnum showKeyword . from

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

