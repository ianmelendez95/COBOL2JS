{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module COBOL.Interpreter
  ( interpretFile 
  ) where 

import Control.Lens hiding (para)

import System.IO

import Control.Monad.State.Lazy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Text.Util
import qualified COBOL.Syntax as S
import qualified COBOL.EBCDIC as E

import Debug.Trace

-- COBOL Interpreter Environment

type CI = StateT CIEnv IO

data CIEnv = CIEnv 
  { _envFds     :: FdMap     -- File Descriptors (FDs) by name
  , _envVars    :: VarMap    -- Variables by name
  , _envFdVars  :: FdVarMap  -- FD to Var bi-directional mapping by name
  , _envParas   :: ParaMap   -- Paragraphs by name
  }

type FdMap    = Map   T.Text Fd
type VarMap   = Map   T.Text Data
type FdVarMap = Bimap T.Text T.Text

type ParaMap  = Map   T.Text S.Para

instance Semigroup CIEnv where 
  (CIEnv fds1 vars1 fvs1 paras1) <> (CIEnv fds2 vars2 fvs2 paras2) = 
    CIEnv (fds1 <> fds2) (vars1 <> vars2) (fvs1 `bimapConcat` fvs2) (paras1 <> paras2)

instance Monoid CIEnv where 
  mempty = CIEnv mempty mempty Bimap.empty mempty

data Value = StrVal T.Text
           | DecVal Double
           | GroupVal [T.Text]
           deriving Show

data Data = StrData Int     T.Text -- length              value
          | DecData Int Int Double -- digits whole-digits value
          | GroupData [T.Text]     -- children
          deriving Show

data Fd = Fd FilePath (Maybe Handle)  -- filename fd-handle

type FileAssoc = [(String, FilePath)]

bimapConcat :: (Ord a, Ord b) => Bimap a b -> Bimap a b -> Bimap a b
bimapConcat m1 m2 = Bimap.fromList (Bimap.toList m1 <> Bimap.toList m2)

makeLenses ''CIEnv

insertValue :: T.Text -> Value -> VarMap -> VarMap
insertValue k v = Map.alter alterVal k
  where 
    alterVal :: Maybe Data -> Maybe Data
    alterVal Nothing      = error $ "Undefined variable: " ++ show k
    alterVal (Just vdata) = Just $ setValue v vdata

insertData :: T.Text -> Data -> VarMap -> VarMap
insertData = Map.insert

dataValue :: Data -> Value
dataValue (StrData _ v)   = StrVal v
dataValue (DecData _ _ v) = DecVal v
dataValue (GroupData v)   = GroupVal v

setValue :: Value -> Data -> Data
setValue (StrVal v) (StrData l _)   = StrData l v
setValue (DecVal n) (DecData d w _) = DecData d w n
setValue v d = error $ "Incompatible types, cannot set (" ++ show d ++ ") to (" ++ show v ++ ")"

getFd :: T.Text -> CI Fd
getFd fd_name = 
  uses envFds    (lookupFailing "No FD for: " fd_name)

getVarNameByFd :: T.Text -> CI T.Text
getVarNameByFd fd_name = 
  uses envFdVars (bilookupFailing "No var for FD: " fd_name)

getVarData :: T.Text -> CI Data
getVarData vname = 
  uses envVars   (lookupFailing "No data for var: " vname)

getVarValue :: T.Text -> CI Value
getVarValue vname = dataValue <$> getVarData vname

lookupFailing :: (Ord a, Show a) => String -> a -> Map a b -> b
lookupFailing msg_pre key m = 
  case Map.lookup key m of 
    Nothing -> error $ msg_pre ++ show key
    Just v  -> v
  
bilookupFailing :: (Ord a, Ord b, Show a) => String -> a -> Bimap a b -> b
bilookupFailing msg_pre key m = 
  case Bimap.lookup key m of 
    Nothing -> error $ msg_pre ++ show key
    Just v  -> v

-- Interpret

interpretFile :: FileAssoc -> FilePath -> IO ()
interpretFile inputs file = S.readFile file >>= interpret inputs

interpret :: FileAssoc -> S.Prog -> IO ()
interpret inputs prog = evalStateT (runProg inputs prog) mempty

runProg :: FileAssoc -> S.Prog -> CI ()
runProg inputs (S.Prog _ env_div data_div proc_div) = do 
  initEnvDiv  inputs env_div
  initDataDiv data_div
  runProcDiv  proc_div

-- Environment Division

initEnvDiv :: FileAssoc -> S.EnvDiv -> CI ()
initEnvDiv inputs = mapM_ initFileCtrl
  where 
    initFileCtrl :: S.FileCtrl -> CI ()
    initFileCtrl (S.FCSelect name src) = 
      case Map.lookup src input_map of 
        Nothing -> error $ "No input associated with name: " ++ show src
        Just fname -> envFds %= Map.insert name (Fd fname Nothing)

    input_map :: Map T.Text String
    input_map = Map.fromList (over (each._1) T.pack inputs)

-- Data Divison

initDataDiv :: S.DataDiv -> CI ()
initDataDiv (S.DataDiv file_descs storage) = do
  mapM_ initFileDescriptor file_descs
  mapM_ initStorageRecord storage

initFileDescriptor :: S.FileDesc -> CI ()
initFileDescriptor (S.FileDesc fd_name record) = do 
  rec_name <- initStorageRecord record
  envFdVars %= Bimap.insert fd_name rec_name

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
runProcDiv proc_div = do 
  mapM_ loadPara proc_div
  mapM_ runPara proc_div

loadPara :: S.Para -> CI T.Text
loadPara para@(S.Para mname _) = do 
  name <- resolveName mname
  envParas %= Map.insert name para
  pure name
  where 
    resolveName :: Maybe T.Text -> CI T.Text
    resolveName (Just name) = pure name
    resolveName Nothing = do
      para_count <- uses envParas Map.size
      pure $ "MAIN-" <> showT para_count

runPara :: S.Para -> CI Bool
runPara (S.Para _ sents) = mapMWhile runSentence sents

runSentence :: S.Sentence -> CI Bool
runSentence = mapMWhile runStatement

runStatement :: S.Statement -> CI Bool
runStatement S.GoBack = pure False  -- TODO - should actually end the program (https://www.ibm.com/docs/en/i/7.3?topic=statements-goback-statement)
runStatement (S.Move val var) = do
  val' <- evalValue val
  envVars %= either (insertValue var) (insertData var) val'
  pure True
runStatement (S.Compute var arith) = do
  arith_val <- runArith arith
  envVars %= insertValue var arith_val
  pure True
runStatement (S.Display svalues) = do
  data_vals <- traverse evalValue svalues
  let txts = map (either valueText dataText) data_vals
  lift $ mapM_ TIO.putStr txts >> putChar '\n'
  pure True
runStatement (S.Open mode name) = runOpenFd mode name >> pure True
runStatement (S.Read fd_name on_eof) = runReadFd fd_name on_eof
runStatement (S.Perform name) = do 
  mpara <- uses envParas (Map.lookup name)
  case mpara of 
    Nothing -> error $ "PERFORM - no paragraph with name: " ++ show name
    Just para -> runPara para
runStatement (S.PerformUntil cond sts) = performUntil cond sts
runStatement s = error $ "statement unsupported: " ++ show s

runOpenFd :: S.IOMode -> T.Text -> CI ()
runOpenFd mode fd_name = do 
  fd <- getFd fd_name
  case fd of 
    (Fd _ (Just _)) -> error $ "OPEN: FD already open: " ++ show fd_name
    (Fd fname Nothing)  -> do
      handle <- lift $ openFile fname io_mode
      envFds %= Map.insert fd_name (Fd fname (Just handle))
  where 
    io_mode :: IOMode
    io_mode = 
      case mode of 
        S.Input  -> ReadMode
        S.Output -> WriteMode

runReadFd :: T.Text -> Maybe [S.Statement] -> CI Bool
runReadFd fd_name msts = do 
  fd <- getFd fd_name
  case fd of 
    (Fd _ Nothing) -> error $ "READ: FD not open: " ++ show fd_name
    (Fd _ (Just handle)) -> do 
      var_name  <- getVarNameByFd fd_name
      vdata     <- getVarData var_name
      readData handle var_name vdata
      handleEOF handle
  where 
    handleEOF :: Handle -> CI Bool
    handleEOF handle = do
      is_eof <- lift $ hIsEOF handle
      if is_eof 
        then maybe (pure True) (mapMWhile runStatement) msts
        else pure True

readData :: Handle -> T.Text -> Data -> CI ()
readData h vname (StrData n _)   = do 
  txt <- lift $ E.readText h n
  envVars %= Map.insert vname (StrData n txt)
readData h vname (DecData d w _) = do
  dec <- lift $ E.readComp3 h d w
  envVars %= Map.insert vname (DecData d w dec)
readData h _ (GroupData child_names)  = do
  mapM_ byName child_names
  where 
    byName :: T.Text -> CI ()
    byName vname = do 
      vdata <- getVarData vname
      readData h vname vdata

performUntil :: S.Cond -> [S.Statement] -> CI Bool
performUntil cond sts = do 
  cond_res <- runCond cond
  if not cond_res
    then pure True
    else do 
      run_res <- runSentence sts
      if not run_res
        then pure False
        else performUntil cond sts

runCond :: S.Cond -> CI Bool
runCond (S.Cond sv1 S.IEq sv2) = do 
  v1 <- evalCValueToValue sv1
  v2 <- evalCValueToValue sv2
  valuesEqual v1 v2
runCond (S.Cond _ cop _) = error $ "Not a valid conditional operator: " ++ show cop  -- caught in parsing

valuesEqual :: Value -> Value -> CI Bool
valuesEqual (StrVal s1) (StrVal s2) = pure $ s1 == s2
valuesEqual (DecVal n1) (DecVal n2) = pure $ n1 == n2
valuesEqual (GroupVal c_ns1) (GroupVal c_ns2) = do
  cs1 <- traverse getVarValue c_ns1
  cs2 <- traverse getVarValue c_ns2
  and <$> zipWithM valuesEqual cs1 cs2
valuesEqual _ _ = pure False

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

-- eval "COBOL" value to internal value
evalCValueToValue :: S.Value -> CI Value
evalCValueToValue v = either id dataValue <$> evalValue v

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


mapMWhile :: (a -> CI Bool) -> [a] -> CI Bool
mapMWhile _ [] = pure True
mapMWhile f (x:xs) = do 
  res <- f x
  if res then mapMWhile f xs else pure False