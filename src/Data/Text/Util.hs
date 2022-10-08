module Data.Text.Util
  ( showT
  , readT
  , failT 
  , lowerFirst
  , kebab2camel
  , kebab2camel'
  ) where 

import qualified Data.Text as T
import Data.List.Split
import Data.Char

showT :: Show a => a -> T.Text
showT = T.pack . show

readT :: Read a => T.Text -> a
readT = read . T.unpack

failT :: MonadFail m => T.Text -> m a
failT = fail . T.unpack

lowerFirst :: T.Text -> T.Text
lowerFirst txt = 
  case T.uncons txt of 
    Nothing -> txt
    Just (c, cs) -> T.cons (toLower c) cs

kebab2camel :: T.Text -> T.Text
kebab2camel = T.pack . kebab2camel' . T.unpack

kebab2camel' :: String -> String
kebab2camel' = mconcat . map strCase . splitOn "-"
  where 
    strCase :: String -> String
    strCase [] = []
    strCase (c:cs) = toUpper c : map toLower cs