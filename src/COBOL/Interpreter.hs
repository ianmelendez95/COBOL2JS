{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module COBOL.Interpreter
  ( interpretFile 
  ) where 

import Control.Lens

import Control.Monad.State.Lazy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text.Util
import qualified COBOL.Syntax as S

type CI = StateT CIEnv IO

data Value = StrVal T.Text
           | DecVal Double
           deriving Show

data Data = StrData Int     T.Text -- length              value
          | DecData Int Int Double -- digits whole-digits value
          | GroupData [T.Text]     -- children
          deriving Show

dataValue :: Data -> Value
dataValue (StrData _ v)   = StrVal v
dataValue (DecData _ _ v) = DecVal v

-- class IsData a where 
--   toData :: a -> Data

-- instance IsData Double where 
--   toData :: Double -> Data
--   toData = 

setValue :: Value -> Data -> Data
setValue (StrVal v) (StrData l _)   = StrData l v
setValue (DecVal n) (DecData d w _) = DecData d w n
setValue v d = error $ "Incompatible types, cannot set (" ++ show d ++ ") to (" ++ show v ++ ")"

type VarMap = Map T.Text Data

newtype CIEnv = CIEnv 
  { _envVars :: VarMap
  }
makeLenses ''CIEnv

instance Semigroup CIEnv where 
  (CIEnv vars1) <> (CIEnv vars2) = CIEnv (vars1 <> vars2)

instance Monoid CIEnv where 
  mempty = CIEnv mempty

lookupData :: T.Text -> VarMap -> Maybe Data
lookupData = Map.lookup
  -- maybe (error $ "Undefined variable: " ++ show v) valueValue (Map.lookup v m)

insertValue :: T.Text -> Value -> VarMap -> VarMap
insertValue k v = Map.alter alterVal k
  where 
    alterVal :: Maybe Data -> Maybe Data
    alterVal Nothing      = error $ "Undefined variable: " ++ show k
    alterVal (Just vdata) = Just $ setValue v vdata

insertData :: T.Text -> Data -> VarMap -> VarMap
insertData = Map.insert

interpretFile :: FilePath -> IO ()
interpretFile file = S.readFile file >>= interpret 

interpret :: S.Prog -> IO ()
interpret prog = evalStateT (runProg prog) mempty

runProg :: S.Prog -> CI ()
runProg (S.Prog _ _ data_div proc_div) = do 
  initDataDiv data_div
  runProcDiv proc_div


-- Data Divison

initDataDiv :: S.DataDiv -> CI ()
initDataDiv (S.DataDiv _ storage) = mapM_ initStorageRecord storage

initStorageRecord :: S.Record -> CI T.Text
initStorageRecord (S.RGroup _ name recs) = do
  child_names <- traverse initStorageRecord recs 
  envVars %= Map.insert name (GroupData child_names)
  pure name
initStorageRecord (S.RElem  _ name fmt msval) = do 
  d <- initial_data
  envVars %= Map.insert name d
  pure name
  where 
    initial_data :: CI Data
    initial_data = maybe fmt_data (`setValue` fmt_data) <$> mval
    
    fmt_data :: Data
    fmt_data = formatToData fmt

    mval :: CI (Maybe Value)
    mval = traverse initialValue msval

    initialValue :: S.Value -> CI Value
    initialValue v = either id dataValue <$> evalValue v

-- TODO improve format checking - check for invalid combinations
formatToData :: S.RFmt -> Data
formatToData (S.RFmt chars S.RDisplay) 
  | S.RFAlphaNum `elem` chars = StrData (count isDisplayChar chars) ""
  | otherwise                 = fmtCharsToDecData chars
  where 
    isDisplayChar :: S.RFmtChar -> Bool
    isDisplayChar S.RFAlphaNum = True
    isDisplayChar S.RFNum      = True
    isDisplayChar S.RFCurrency = True
    isDisplayChar S.RFDecPer   = True
    isDisplayChar S.RFComma    = True
    isDisplayChar _ = False

formatToData (S.RFmt chars S.RComp3) = fmtCharsToDecData chars

-- http://www.3480-3590-data-conversion.com/article-packed-fields.html
fmtCharsToDecData :: [S.RFmtChar] -> Data
fmtCharsToDecData chars = 
  DecData (halfRoundUp $ count isDigitChar chars) 
          (count isDigitChar (takeWhile (not . isDecimalChar) chars))
          0.0
  where 
    isDigitChar :: S.RFmtChar -> Bool
    isDigitChar S.RFNum = True
    isDigitChar _ = False

    isDecimalChar :: S.RFmtChar -> Bool
    isDecimalChar S.RFDec = True
    isDecimalChar _ = False

    halfRoundUp :: Int -> Int
    halfRoundUp x = ceiling (fromIntegral x / (2 :: Double))

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f 


-- Procedure Division

runProcDiv :: S.ProcDiv -> CI ()
runProcDiv = mapM_ runPara

runPara :: S.Para -> CI ()
runPara (S.Para _ sents) = mapM_ runSentence sents

runSentence :: S.Sentence -> CI ()
runSentence = mapM_ runStatement

runStatement :: S.Statement -> CI ()
runStatement S.GoBack = pure ()
runStatement (S.Move val var) = do
  val' <- evalValue val
  envVars %= either (insertValue var) (insertData var) val'
runStatement (S.Compute var arith) = do
  arith_val <- runArith arith
  envVars %= insertValue var arith_val
runStatement (S.Display svalues) = do
  data_vals <- traverse evalValue svalues
  let txts = map (either valueText dataText) data_vals
  lift $ mapM_ TIO.putStr txts >> putChar '\n'

varData :: T.Text -> CI Data
varData var = do 
  vdata <- uses envVars (Map.lookup var)
  pure $ fromMaybe (error $ "Undefined variable: " ++ show var) vdata

runArith :: S.Arith -> CI Value
runArith (S.AVal val) = either id dataValue <$> evalValue val
runArith (S.ABin a1 aop a2) = 
  arithOpFunc aop <$> runArith a1 
                  <*> runArith a2
  where
    arithOpFunc :: S.IOp -> (Value -> Value -> Value)
    arithOpFunc S.Mult = numValueFunc (*)  
    arithOpFunc S.Add  = numValueFunc (+)  
    arithOpFunc aop'  = error $ "Arithmetic does not support '" ++ show aop' ++ "' operator"

    numValueFunc :: (Double -> Double -> Double) -> Value -> Value -> Value
    numValueFunc f (DecVal x) (DecVal y) = DecVal $ f x y
    numValueFunc _ (DecVal _) v = error $ "Second argument is not a number: " ++ show v
    numValueFunc _ v _          = error $ "First argument is not a number: "  ++ show v

evalValue :: S.Value -> CI (Either Value Data)
evalValue (S.StrVal str) = pure . Left $ StrVal str
evalValue (S.NumVal num) = pure . Left $ DecVal num
evalValue (S.VarVal var) = do 
  mdata <- uses envVars (Map.lookup var)
  pure . Right $ fromMaybe (error $ "Undefined variable: " ++ show var) mdata

dataText :: Data -> T.Text
dataText (StrData l v)   = rightPad l v
dataText (DecData _ _ v) = showT v

rightPad :: Int -> T.Text -> T.Text
rightPad n txt 
  | n > len   = txt <> T.replicate (n - len) " " 
  | otherwise = txt
  where 
    len = T.length txt

valueText :: Value -> T.Text
valueText (StrVal v) = v
valueText (DecVal v) = showT v

