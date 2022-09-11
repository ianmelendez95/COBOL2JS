{-# LANGUAGE OverloadedStrings #-}

module JS 
  ( Script (..)
  , Statement (..)
  , Expr (..)
  , UOp (..)
  , IOp (..)

  , Value (..)
  , Obj
  , Arr
  , scriptToText
  ) where 

import Prelude hiding (unlines)
import qualified Data.Text as T hiding (unlines)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intersperse, intercalate)
import Data.Text.Util

newtype Script = Script [Statement] deriving Show

data Statement = Expr Expr
               | While Expr [Statement]
               | Return Expr
               deriving Show

data Expr = Raw T.Text
          | Func T.Text [Statement]
          | Call Value [Value]
          | Log [Value]
          | Set T.Text Expr
          | Val Value
          | Infix Expr IOp Expr
          | Unary UOp Expr
          deriving Show

data UOp = UNot

data IOp = Mult 
         | Add
         | IEq

data Value = StrVal T.Text
           | VarVal T.Text 
           | NumVal Int
           | ObjVal Obj
           | ArrVal Arr
           | FunVal T.Text [Statement]
           deriving Show

type Obj = Map T.Text Value
type Arr = [Value]

instance Show UOp where 
  show UNot = "!"

instance Show IOp where 
  show Mult = "*"
  show Add  = "+"
  show IEq  = "="

scriptToText :: Script -> T.Text
scriptToText (Script statements) = unlinesT . map statementToText $ statements

statementToText :: Statement -> T.Text
statementToText (Expr e) = exprToText e
statementToText (While cond body) = unlinesT $
  [ "while (" <> exprToText cond <> ") {" ] <> map statementToText body <> [ "}" ]
statementToText (Return e) = "return " <> exprToText e

exprToText :: Expr -> T.Text
exprToText (Raw t) = t
exprToText (Func name body) = 
  let body_text :: [T.Text]
      body_text = map statementToText body
  in unlinesT $ 
    [ "function " <> name <> "() {" ] 
    ++ body_text ++ 
    [ "}" ]
exprToText (Call val args) = valToText val <> "(" <> argsToText args <> ")"
  where 
    argsToText :: [Value] -> T.Text
    argsToText = mconcat . intersperse ", " . map (valToText)
exprToText (Log vals) = "console.log(" <> args <> ")" 
  where 
    args = T.concat . intersperse ", " . map valToText $ vals
exprToText (Set var val) = var <> " = " <> exprToText val
exprToText (Val val) = valToText val
exprToText (Infix l op r) = "(" <> exprToText l <> showT op <> exprToText r <> ")"
exprToText (Unary op e) = showT op <> "(" <> exprToText e <> ")"

-- arithToScript :: Arith -> T.Text
-- arithToScript (AVal v) = valToScript v
-- arithToScript (ABin a1 op a2) = 
--   "(" <> arithToScript a1 <> " " <> showT op <> " " <> arithToScript a2 <> ")"

valToText :: Value -> T.Text
valToText (StrVal str) = "\"" <> str <> "\""
valToText (VarVal var) = var
valToText (NumVal num) = T.pack . show $ num
valToText (ObjVal entries) = unlinesT $ 
  [ "{" ] <> map showEntry (Map.toList entries) <> [ "}" ]
  where 
    showEntry :: (T.Text, Value) -> T.Text
    showEntry (k, v) = k <> ": " <> valToText v <> ","
valToText (ArrVal arr) = unlinesT $ 
  [ "[" ] <> map (\v -> valToText v <> ", ") arr <> [ "]" ]
valToText (FunVal name body) = unlinesT $ 
  [ "function " <> name <> "() {" ] <> map statementToText body <> [ "}" ]

unlinesT :: [T.Text] -> T.Text
unlinesT = mconcat . intersperse "\n"
