{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module COBOL2JS 
  ( c2jFile
  ) where 

import Control.Lens

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (singleton)
import Data.List.Split
import Data.Maybe
import Data.Char

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Util

import Control.Monad.State

import qualified COBOL
import qualified COBOL.Keyword
import qualified JS

-- import Debug.Trace

trace :: String -> a -> a
trace _ = id

-- VarSpec

data VarSpec = VSComp T.Text [VarSpec]
             | VSData T.Text VarSpecFmt (Maybe JS.Value)

data VarSpecFmt = VSFString Int
                | VSFBinDec Int Int  -- length wholeNumbers

varSpecName :: VarSpec -> T.Text
varSpecName (VSComp name _) = name
varSpecName (VSData name _ _) = name

-- C2J State

type C2J = State C2JEnv

type VarMap = Map T.Text T.Text

type FileRecMap = Map T.Text T.Text

data C2JEnv = C2JEnv 
  { _envVarMap :: VarMap
  , _fileDescs  :: FileRecMap  -- record var => file descriptor
  }

makeLenses ''C2JEnv

instance Semigroup C2JEnv where 
  (C2JEnv vars1 files1) <> (C2JEnv vars2 files2) = C2JEnv (vars1 <> vars2) (files1 <> files2)

instance Monoid C2JEnv where 
  mempty = C2JEnv mempty mempty


c2jFile :: FilePath -> FilePath -> IO ()
c2jFile cobolSrc jsDest = do
  cobol_text <- COBOL.readFile cobolSrc
  let js_script = evalState (c2j cobol_text) mempty
  TIO.writeFile jsDest (JS.scriptToText js_script)

c2j :: COBOL.Prog -> C2J JS.Script
c2j (COBOL.Prog _ env data_div proc_div) = do 
  let file_ctrl = map fileCtrl2js env
  data_sts <- dataDiv2js data_div
  (proc_names, procs) <- procedureDiv2js proc_div
  pure . JS.Script $ file_ctrl <> data_sts <> procs <> procsRunner proc_names

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
    , var2js' fd 
    , " = new FileDescriptor(process.env[\""
    , env_var 
    , "\"])"
    ]

dataDiv2js :: COBOL.DataDiv -> C2J [JS.Statement]
dataDiv2js (COBOL.DataDiv files storage) = do
  file_desc  <- traverse fileDesc2js files
  store_vars <- traverse record2jsVarDec storage
  pure $ file_desc <> store_vars

-- concatPairs :: [(a,b)] -> ([a],[b])
-- concatPairs = 

record2jsVarDec :: COBOL.Record -> C2J JS.Statement
record2jsVarDec record = 
  let var_spec = record2VarSpec record
   in do 
        envVarMap <>= varSpec2VarMap var_spec
        pure . JS.Expr $ JS.VarDec JS.Let (varSpecName var_spec) 
          (Just (JS.Call (JS.VarVal "_valueFromVarSpec") 
                         [JS.ObjVal $ varSpec2js var_spec]))

fileDesc2js :: COBOL.FileDesc -> C2J JS.Statement
fileDesc2js (COBOL.FileDesc name record) = 
  let js_name = var2js' name
      var_spec = record2VarSpec record
      load_call = JS.Call (JS.VarVal $ js_name <> "._loadVarSpec")
                          [JS.ObjVal $ varSpec2js var_spec]
   in do 
        fileDescs  %=  Map.insert (varSpecName var_spec) js_name
        envVarMap <>= prefixVarMap js_name (varSpec2VarMap var_spec)
        pure $ JS.Expr load_call

varSpec2VarMap :: VarSpec -> Map T.Text T.Text
varSpec2VarMap (VSComp name specs) = 
  prefixVarMap name (foldMap varSpec2VarMap specs)
varSpec2VarMap (VSData name _ _) = Map.singleton name name

prefixVarMap :: T.Text -> VarMap -> VarMap
prefixVarMap prefix = fmap (\v -> prefix <> "." <> v) 

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
  VSComp (var2js' name) (map record2VarSpec recs)
record2VarSpec (COBOL.RElem _ name fmt mvalue) = 
  VSData (var2js' name) (fmt2VarSpecCont fmt) (c2jVal' <$> mvalue)

fmt2VarSpecCont :: COBOL.RFmt -> VarSpecFmt
fmt2VarSpecCont (COBOL.RFmt chars COBOL.RDisplay) = 
  VSFString (count isDisplayChar chars)
  where 
    isDisplayChar :: COBOL.RFmtChar -> Bool
    isDisplayChar COBOL.RFAlphaNum = True
    isDisplayChar COBOL.RFNum      = True
    isDisplayChar COBOL.RFCurrency = True
    isDisplayChar COBOL.RFDecPer   = True
    isDisplayChar COBOL.RFComma    = True
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
procedureDiv2js :: COBOL.ProcDiv -> C2J ([T.Text], [JS.Statement])
procedureDiv2js paras = do
  sts <- traverse paragraph2js (zip [0..] paras)
  pure $ foldMap bothSingle sts
  where 
    bothSingle :: (a, b) -> ([a], [b])
    bothSingle (x,y) = (singleton x, singleton y)

paragraph2js :: (Int, COBOL.Para) -> C2J (T.Text, JS.Statement)
paragraph2js (n, COBOL.Para mname sts) = do
  js_sts <- concat <$> traverse sentence2js sts
  name   <- maybe (pure $ "main_" <> showT n) var2js mname
  pure (name, JS.Expr $ JS.Func name js_sts) 

sentence2js :: COBOL.Sentence -> C2J [JS.Statement]
sentence2js = traverse statement2js

statement2js :: COBOL.Statement -> C2J JS.Statement
statement2js (COBOL.Display vs) = JS.Expr . JS.Log <$> traverse c2jVal vs
statement2js (COBOL.Move val var) = JS.Expr <$> (JS.Set <$> var2js var <*> value2js val)
statement2js (COBOL.Compute var val) = JS.Expr <$> (JS.Set <$> var2js var <*> arithmetic2js val)
statement2js (COBOL.Open put_io var) = 
  JS.Expr <$> (JS.Call <$> (JS.VarVal <$> (var2js var <&> (<> "._open"))) 
                       <*> pure [JS.StrVal (putFlags put_io)])
  where 
    putFlags :: COBOL.Put -> T.Text
    putFlags COBOL.Input  = "r"
    putFlags COBOL.Output = "w"
statement2js (COBOL.Close var) = JS.Expr <$> (JS.Call <$> (JS.VarVal <$> (var2js var <&> (<> "._close"))) <*> pure [])
statement2js (COBOL.Read var mstatements) = 
  let at_end_arg :: C2J [JS.Value]
      at_end_arg = maybe (pure []) statements2func mstatements
   in JS.Expr <$> (JS.Call <$> (JS.VarVal <$> (var2js var <&> (<> "._read"))) 
                           <*> at_end_arg)
  where 
    statements2func :: [COBOL.Statement] -> C2J [JS.Value]
    statements2func sts = singleton . JS.FunVal "" <$> traverse statement2js sts
statement2js (COBOL.Write var) = do
  js_var <- var2js var
  mfd <- uses fileDescs (Map.lookup js_var) 
  case mfd of 
    Nothing -> error $ "No associated file descriptor for var: " ++ T.unpack var
    Just fd -> pure $ JS.Expr (JS.Call (JS.VarVal (fd <> "._write")) [])
statement2js (COBOL.Perform var) = 
  JS.Expr <$> (JS.Call <$> (JS.VarVal <$> var2js var) <*> pure [])
statement2js (COBOL.PerformUntil cond sts) = 
  JS.While <$> (JS.Unary JS.UNot <$> condition2js cond) <*> traverse statement2js sts
statement2js COBOL.GoBack = pure $ JS.Return (JS.Val (JS.StrVal (showT COBOL.Keyword.KGoback)))

condition2js :: COBOL.Cond -> C2J JS.Expr
condition2js (COBOL.Cond v1 cop v2) = 
  JS.Infix <$> value2js v1 <*> pure (op2js cop) <*> value2js v2

value2js :: COBOL.Value -> C2J JS.Expr
value2js cval = JS.Val <$> c2jVal cval

c2jVal :: COBOL.Value -> C2J JS.Value
c2jVal = c2jValWithVars var2js

c2jVal' :: COBOL.Value -> JS.Value
c2jVal' = runIdentity . c2jValWithVars Identity

c2jValWithVars :: Monad m => (T.Text -> m T.Text) -> COBOL.Value -> m JS.Value
c2jValWithVars var_f (COBOL.VarVal v) = JS.VarVal <$> var_f v
c2jValWithVars _     (COBOL.NumVal v) = pure $ JS.NumVal v
c2jValWithVars _     (COBOL.StrVal v) = pure $ JS.StrVal v

var2js :: T.Text -> C2J T.Text
var2js cobol_var = 
  let camel_raw = var2js' cobol_var
   in fromMaybe camel_raw <$> uses envVarMap (Map.lookup camel_raw)

-- non monadic version (does not lookup var mapping)
var2js' :: T.Text -> T.Text
var2js' = lowerFirst . kebab2camel

arithmetic2js :: COBOL.Arith -> C2J JS.Expr
arithmetic2js (COBOL.AVal v) = value2js v
arithmetic2js (COBOL.ABin a1 aop a2) = 
  JS.Infix <$> arithmetic2js a1 <*> pure (op2js aop) <*> arithmetic2js a2
       
op2js :: COBOL.IOp -> JS.IOp
op2js COBOL.Mult = JS.Mult
op2js COBOL.Add = JS.Add
op2js COBOL.IEq = JS.IEq