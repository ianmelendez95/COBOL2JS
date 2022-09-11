{-# LANGUAGE OverloadedStrings #-}

module COBOL2JS 
  ( c2jFile
  ) where 

import qualified COBOL
import qualified COBOL.Keyword
import qualified JS

-- import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (singleton)
import Data.List.Split
import Data.Maybe
import Data.Char

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Util

data VarSpec = VSComp T.Text [VarSpec]
             | VSData T.Text VarSpecFmt

data VarSpecFmt = VSFString Int
                | VSFBinDec Int Int

c2jFile :: FilePath -> FilePath -> IO ()
c2jFile cobolSrc jsDest = do
  jsScript <- c2j <$> COBOL.readFile cobolSrc
  TIO.writeFile jsDest (JS.scriptToText jsScript)

c2j :: COBOL.Prog -> JS.Script
c2j (COBOL.Prog _ env data_div paras) = 
  JS.Script $  map fileCtrl2js env
            <> map fileDesc2js (COBOL.dataFiles data_div)
            <> concatMap paragraph2js paras

fileCtrl2js :: COBOL.FileCtrl -> JS.Statement
fileCtrl2js (COBOL.FCSelect fd env_var) = 
  JS.Expr . JS.Raw $ T.concat 
    [ "let "
    , var2js fd 
    , " = new FileDescriptor(process.env[\""
    , env_var 
    , "\"])"
    ]

fileDesc2js :: COBOL.FileDesc -> JS.Statement
fileDesc2js (COBOL.FileDesc name record) = 
  JS.Expr $ JS.Call 
    (JS.VarVal $ var2js name <> ".loadVarSpec")
    [JS.ObjVal (varSpec2js . record2VarSpec $ record)]

varSpec2js :: VarSpec -> JS.Obj
varSpec2js (VSComp name specs) = Map.fromList 
  [ ("name", JS.StrVal name)
  , ("type", JS.StrVal "compound")
  , ( "children"
    , JS.ArrVal (map (JS.ObjVal . varSpec2js) specs))
  ]
varSpec2js (VSData name fmt) = Map.fromList $
  [ ("name", JS.StrVal name)
  ] <> fmtProps fmt
  where 
    fmtProps :: VarSpecFmt -> [(T.Text, JS.Value)]
    fmtProps (VSFString len) = 
      [ ("type",   JS.StrVal "string")
      , ("length", JS.NumVal len)
      ]
    fmtProps (VSFBinDec len whole_digits) = 
      [ ("type",        JS.StrVal "binary-decimal")
      , ("length",      JS.NumVal len)
      , ("wholeDigits", JS.NumVal whole_digits)
      ]


record2VarSpec :: COBOL.Record -> VarSpec
record2VarSpec (COBOL.RGroup _ name recs) = 
  VSComp (var2js name) (map record2VarSpec recs)
record2VarSpec (COBOL.RElem _ name fmt) = 
  VSData (var2js name) (fmt2VarSpecFmt fmt)

fmt2VarSpecFmt :: COBOL.RFmt -> VarSpecFmt

fmt2VarSpecFmt fmt@(COBOL.RFmt _ _ (Just _)) = 
  error $ "Initial values in file section not implemented: " <> show fmt

fmt2VarSpecFmt (COBOL.RFmt chars COBOL.RDisplay _) = 
  VSFString (count isDisplayChar chars)
  where 
    isDisplayChar :: COBOL.RFmtChar -> Bool
    isDisplayChar COBOL.RFAlphaNum = True
    isDisplayChar _ = False

-- http://www.3480-3590-data-conversion.com/article-packed-fields.html
fmt2VarSpecFmt (COBOL.RFmt chars COBOL.RComp3 _) = 
  VSFBinDec (halfRoundUp $ count isDigitChar chars) 
            (count isDigitChar (takeWhile (not . isDecimalChar) chars))
  where 
    isDigitChar :: COBOL.RFmtChar -> Bool
    isDigitChar COBOL.RFNum = True
    isDigitChar _ = False

    isDecimalChar :: COBOL.RFmtChar -> Bool
    isDecimalChar COBOL.RFDec = True
    isDecimalChar _ = False

    halfRoundUp :: Int -> Int
    halfRoundUp x = ceiling (fromIntegral x / (2 :: Double))

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f 

paragraph2js :: COBOL.Para -> [JS.Statement]
paragraph2js (COBOL.Para mname sts) = 
  let js_sts = concatMap sentence2js sts
   in case mname of 
        Nothing -> js_sts
        Just name -> [JS.Expr $ JS.Func (var2js name) js_sts]

sentence2js :: COBOL.Sentence -> [JS.Statement]
sentence2js = map statement2js

statement2js :: COBOL.Statement -> JS.Statement
statement2js (COBOL.Display vs) = JS.Expr $ JS.Log (map c2jVal vs)
statement2js (COBOL.Move val var) = JS.Expr $ JS.Set (var2js var) (value2js val)
statement2js (COBOL.Compute var val) = JS.Expr $ JS.Set (var2js var) (arithmetic2js val)
statement2js (COBOL.Open put var) = 
  JS.Expr $ JS.Call (JS.VarVal $ var2js var <> ".open") 
                    [JS.StrVal (putFlags put)]
  where 
    putFlags :: COBOL.Put -> T.Text
    putFlags COBOL.Input  = "r"
    putFlags COBOL.Output = "w"
statement2js (COBOL.Close var) = JS.Expr $ JS.Call (JS.VarVal $ var2js var <> ".close") []
statement2js (COBOL.Read var mstatements) = 
  JS.Expr $ JS.Call (JS.VarVal $ var2js var <> ".read") 
                    (maybe [] (singleton . statements2func) mstatements)
  where 
    statements2func :: [COBOL.Statement] -> JS.Value
    statements2func = JS.FunVal "" . map statement2js
statement2js (COBOL.Write var) = JS.Expr $ JS.Call (JS.VarVal $ var2js var <> ".write") []
statement2js (COBOL.Perform var) = JS.Expr $ JS.Call (JS.VarVal $ var2js var) []
statement2js (COBOL.PerformUntil cond sts) = 
  JS.While (JS.Unary JS.UNot (condition2js cond)) (map statement2js sts)
statement2js COBOL.GoBack = JS.Return (JS.Val (JS.StrVal (showT COBOL.Keyword.KGoback)))

condition2js :: COBOL.Cond -> JS.Expr
condition2js (COBOL.Cond v1 op v2) = JS.Infix (value2js v1) (op2js op) (value2js v2)

value2js :: COBOL.Value -> JS.Expr
value2js = JS.Val . c2jVal

c2jVal :: COBOL.Value -> JS.Value
c2jVal (COBOL.VarVal v) = JS.VarVal (var2js v)
c2jVal (COBOL.NumVal v) = JS.NumVal v
c2jVal (COBOL.StrVal v) = JS.StrVal v

var2js :: T.Text -> T.Text
var2js = lowerFirst . kebab2camel

arithmetic2js :: COBOL.Arith -> JS.Expr
arithmetic2js (COBOL.AVal v) = value2js v
arithmetic2js (COBOL.ABin a1 op a2) = JS.Infix (arithmetic2js a1) (op2js op) (arithmetic2js a2)
       
op2js :: COBOL.IOp -> JS.IOp
op2js COBOL.Mult = JS.Mult
op2js COBOL.Add = JS.Add
op2js COBOL.IEq = JS.IEq