{-# LANGUAGE OverloadedStrings #-}

module JS where 

import qualified Data.Text as T
import Data.List

newtype Script = Script [Statement] deriving Show

data Statement = Log [Value]
               | Set T.Text Arith
               deriving Show

data Arith = AVal Value
           | Arith Arith AOp Arith
           deriving Show

data AOp = Mult 
         | Add

data Value = StrVal T.Text
           | VarVal T.Text 
           | NumVal Int
           deriving Show

instance Show AOp where 
  show Mult = "*"
  show Add  = "+"

showT :: Show a => a -> T.Text
showT = T.pack . show

scriptToText :: Script -> T.Text
scriptToText (Script statements) = T.unlines . map statementToText $ statements

statementToText :: Statement -> T.Text
statementToText (Log vals) = "console.log(" <> args <> ")" 
  where 
    args = T.concat . intersperse ", " . map valToScript $ vals
statementToText (Set var val) = var <> " = " <> arithToScript val

arithToScript :: Arith -> T.Text
arithToScript (AVal v) = valToScript v
arithToScript (Arith a1 op a2) = 
  "(" <> arithToScript a1 <> " " <> showT op <> " " <> arithToScript a2 <> ")"

valToScript :: Value -> T.Text
valToScript (StrVal str) = "\"" <> str <> "\""
valToScript (VarVal var) = var
valToScript (NumVal num) = T.pack . show $ num
