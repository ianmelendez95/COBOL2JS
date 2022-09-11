{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
             | VSData T.Text VarSpecFmt (Maybe JS.Value)

data VarSpecFmt = VSFString Int
                | VSFBinDec Int Int

varSpecName :: VarSpec -> T.Text
varSpecName (VSComp name _) = name
varSpecName (VSData name _ _) = name

c2jFile :: FilePath -> FilePath -> IO ()
c2jFile cobolSrc jsDest = do
  jsScript <- c2j <$> COBOL.readFile cobolSrc
  TIO.writeFile jsDest (JS.scriptToText jsScript)

c2j :: COBOL.Prog -> JS.Script
c2j (COBOL.Prog _ env data_div proc_div) = do 
  let file_ctrl = map fileCtrl2js env
      data_sts = dataDiv2js data_div
      (proc_names, procs) = procedureDiv2js proc_div
   in JS.Script $ file_ctrl <> data_sts <> procs <> procsRunner proc_names

procsRunner :: [T.Text] -> [JS.Statement]
procsRunner proc_names = 
  [ JS.Expr $ JS.VarDec JS.Const "__PROCEDURES__" (Just (JS.Val . JS.ArrVal $ procs_arr))
  , JS.Expr $ JS.Call (JS.VarVal "_runProcedures") [JS.VarVal "__PROCEDURES__"]
  ]
  where 
    procs_arr :: JS.Arr
    procs_arr = map JS.VarVal proc_names

fileCtrl2js :: COBOL.FileCtrl -> JS.Statement
fileCtrl2js (COBOL.FCSelect fd env_var) = 
  JS.Expr . JS.Raw $ T.concat 
    [ "let "
    , var2js fd 
    , " = new FileDescriptor(process.env[\""
    , env_var 
    , "\"])"
    ]

dataDiv2js :: COBOL.DataDiv -> [JS.Statement]
dataDiv2js (COBOL.DataDiv files storage) = 
  let file_desc = map fileDesc2js files
      store_vars = map record2jsVarDec storage
   in file_desc <> store_vars

record2jsVarDec :: COBOL.Record -> JS.Statement
record2jsVarDec record = 
  let spec = record2VarSpec record
   in JS.Expr $ JS.VarDec JS.Let (varSpecName spec) 
        (Just (JS.Call (JS.VarVal "_valueFromVarSpec") 
                       [JS.ObjVal $ varSpec2js spec]))

fileDesc2js :: COBOL.FileDesc -> JS.Statement
fileDesc2js (COBOL.FileDesc name record) = 
  JS.Expr $ JS.Call 
    (JS.VarVal $ var2js name <> "._loadVarSpec")
    [JS.ObjVal (varSpec2js . record2VarSpec $ record)]

varSpec2js :: VarSpec -> JS.Obj
varSpec2js (VSComp name specs) = Map.fromList
  [ ("name", JS.StrVal name) 
  , ("type", JS.StrVal "compound")
  , ( "children"
    , JS.ArrVal (map (JS.ObjVal . varSpec2js) specs)
    )
  ]
varSpec2js (VSData name cont mval) = 
  Map.fromList $ 
    [ ("name", JS.StrVal name) ] ++ 
    varSpecFmt2Props cont ++
    maybe [] (singleton . ("initialValue",)) mval

varSpecFmt2Props :: VarSpecFmt -> [(T.Text, JS.Value)]
varSpecFmt2Props (VSFString len) = 
  [ ("type",   JS.StrVal "string")
  , ("length", JS.NumVal len)
  ]
varSpecFmt2Props (VSFBinDec len whole_digits) = 
  [ ("type",        JS.StrVal "binary-decimal")
  , ("length",      JS.NumVal len)
  , ("wholeDigits", JS.NumVal whole_digits)
  ]

record2VarSpec :: COBOL.Record -> VarSpec
record2VarSpec (COBOL.RGroup _ name recs) = 
  VSComp (var2js name) (map record2VarSpec recs)
record2VarSpec (COBOL.RElem _ name fmt mvalue) = 
  VSData (var2js name) (fmt2VarSpecCont fmt) (c2jVal <$> mvalue)

fmt2VarSpecCont :: COBOL.RFmt -> VarSpecFmt
fmt2VarSpecCont (COBOL.RFmt chars COBOL.RDisplay) = 
  VSFString (count isDisplayChar chars)
  where 
    isDisplayChar :: COBOL.RFmtChar -> Bool
    isDisplayChar COBOL.RFAlphaNum = True
    isDisplayChar _ = False

-- http://www.3480-3590-data-conversion.com/article-packed-fields.html
fmt2VarSpecCont (COBOL.RFmt chars COBOL.RComp3) = 
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

-- Return (function names, statements)
procedureDiv2js :: COBOL.ProcDiv -> ([T.Text], [JS.Statement])
procedureDiv2js paras = 
  foldMap (bothSingle . paragraph2js) (zip [0..] paras)
  where 
    bothSingle :: (a, b) -> ([a], [b])
    bothSingle (x,y) = (singleton x, singleton y)

paragraph2js :: (Int, COBOL.Para) -> (T.Text, JS.Statement)
paragraph2js (n, COBOL.Para mname sts) = 
  let js_sts = concatMap sentence2js sts
      name = maybe ("main_" <> showT n) var2js mname
   in (name, JS.Expr $ JS.Func name js_sts) 

sentence2js :: COBOL.Sentence -> [JS.Statement]
sentence2js = map statement2js

statement2js :: COBOL.Statement -> JS.Statement
statement2js (COBOL.Display vs) = JS.Expr $ JS.Log (map c2jVal vs)
statement2js (COBOL.Move val var) = JS.Expr $ JS.Set (var2js var) (value2js val)
statement2js (COBOL.Compute var val) = JS.Expr $ JS.Set (var2js var) (arithmetic2js val)
statement2js (COBOL.Open put var) = 
  JS.Expr $ JS.Call (JS.VarVal $ var2js var <> "._open") 
                    [JS.StrVal (putFlags put)]
  where 
    putFlags :: COBOL.Put -> T.Text
    putFlags COBOL.Input  = "r"
    putFlags COBOL.Output = "w"
statement2js (COBOL.Close var) = JS.Expr $ JS.Call (JS.VarVal $ var2js var <> "._close") []
statement2js (COBOL.Read var mstatements) = 
  JS.Expr $ JS.Call (JS.VarVal $ var2js var <> "._read") 
                    (maybe [] (singleton . statements2func) mstatements)
  where 
    statements2func :: [COBOL.Statement] -> JS.Value
    statements2func = JS.FunVal "" . map statement2js
statement2js (COBOL.Write var) = JS.Expr $ JS.Call (JS.VarVal $ var2js var <> "._write") []
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