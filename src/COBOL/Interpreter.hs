{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module COBOL.Interpreter
  ( interpretFile 

  , _test_report
  ) where 

import Control.Lens hiding (para)

import System.IO
    ( hClose, hIsEOF, openFile, Handle, IOMode(WriteMode, ReadMode) )

import Control.Monad.State.Strict
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Maybe

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Text.Util
import qualified COBOL.Syntax as S
import qualified COBOL.EBCDIC as E

import Numeric hiding (readDec)

import Data.Time
import Data.Time.LocalTime

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

data Data = StrData [S.RFmtChar] Int     T.Text -- length              value
          | DecData [S.RFmtChar] Int Int Double -- bin-length whole-digits value
          | GroupData [T.Text]     -- children
          deriving Show

data Fd = Fd FilePath (Maybe Handle)  -- filename fd-handle

type FileAssoc = [(String, FilePath)]

type StrValReader = StateT T.Text CI

svrTake :: Int -> StrValReader T.Text
svrTake n = do 
  result <- state (T.splitAt n)
  if T.length result < n
    then error "Insufficient text for reading"
    else pure result

bimapConcat :: (Ord a, Ord b) => Bimap a b -> Bimap a b -> Bimap a b
bimapConcat m1 m2 = Bimap.fromList (Bimap.toList m1 <> Bimap.toList m2)

makeLenses ''CIEnv

insertValue :: T.Text -> Value -> CI ()
insertValue k (StrVal v) = evalStateT (insertStringValue k) v
insertValue k src@(DecVal v) = do 
  d <- getVarData k
  case d of 
    (DecData f l w _) -> 
      envVars %= Map.insert k (DecData f l w v)
    _ -> error $ "Cannot move (" ++ show src ++ ") into (" ++ show d ++ ")"
insertValue k src@(GroupVal _) = 
  error $ "No support for moving group data (" ++ show src ++ ") into (" ++ show k ++ ")"

-- | returns the rest of the unparsed string
-- TODO throw errors for insufficient string lengths
insertStringValue :: T.Text -> StrValReader ()
insertStringValue vname = do 
  d <- lift $ getVarData vname 
  case d of 
    (StrData f l _)   -> do 
      result <- StrData f l <$> svrTake l
      lift (envVars %= Map.insert vname result)
    (DecData f l w _) -> do
      result <- DecData f l w <$> readDec f
      lift (envVars %= Map.insert vname result)
    (GroupData cs) -> mapM_ insertStringValue cs
  where 
    readDec :: [S.RFmtChar] -> StrValReader Double
    readDec fcs
      | not (all (== S.RFNum) fcs) = 
          error $ "Only support moving alphanum to simple integers - got: " ++ show fcs
      | otherwise = readT <$> svrTake (length fcs)

dataValue :: Data -> Value
dataValue (StrData _ _ v)   = StrVal v
dataValue (DecData _ _ _ v) = DecVal v
dataValue (GroupData v)   = GroupVal v

getFd :: T.Text -> CI Fd
getFd fd_name = 
  uses envFds    (lookupFailing "No FD for: " fd_name)

getFdByVarName :: T.Text -> CI Fd
getFdByVarName var_name = getFdNameByVar var_name >>= getFd 

getFdNameByVar :: T.Text -> CI T.Text
getFdNameByVar var_name = 
  uses envFdVars (bilookupFailingR "No FD for var: " var_name)

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

bilookupFailingR :: (Ord a, Ord b, Show b) => String -> b -> Bimap a b -> a
bilookupFailingR msg_pre key m = 
  case Bimap.lookupR key m of 
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

    setValue :: Value -> Data -> Data
    setValue (StrVal v) (StrData f l _)   = StrData f l (T.take l v)
    setValue (DecVal n) (DecData f d w _) = DecData f d w n
    setValue v d = error $ "Incompatible initial value, cannot set (" ++ show d ++ ") to (" ++ show v ++ ")"
    
    fmt_data :: Data
    fmt_data = formatToData fmt

    mval :: CI (Maybe Value)
    mval = traverse initialValue msval

    initialValue :: S.Value -> CI Value
    initialValue v = either id dataValue <$> evalValue v

-- TODO improve format checking - check for invalid combinations
formatToData :: S.RFmt -> Data
formatToData (S.RFmt chars S.RDisplay) 
  | S.RFAlphaNum `elem` chars = StrData chars (count isDisplayChar chars) ""
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
  DecData chars
          (halfRoundUp $ count isDigitChar chars) 
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
  mapMWhile_ runPara proc_div

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
runSentence = mapMWhile (\s -> traceM ("STATEMENT: " ++ show s) >> runStatement s)

runStatement :: S.Statement -> CI Bool
runStatement S.GoBack = pure False  -- TODO - should actually end the program (https://www.ibm.com/docs/en/i/7.3?topic=statements-goback-statement)
runStatement (S.Move sval var) = do
  val <- evalValue sval
  insertValue var (either id dataValue val)
  pure True
runStatement (S.Compute var arith) = do
  arith_val <- runArith arith
  insertValue var arith_val
  pure True
runStatement (S.Display svalues) = do
  data_vals <- traverse evalValue svalues
  txts <- traverse (either valueText dataText) data_vals
  lift $ mapM_ TIO.putStr txts >> putChar '\n'
  pure True
runStatement (S.Open mode name) = runOpenFd mode name >> pure True
runStatement (S.Close name)     = runCloseFd name     >> pure True
runStatement (S.Read fd_name on_eof) = runReadFd fd_name on_eof
runStatement (S.Write var mfrom madv) = runWrite var mfrom madv >> pure True
runStatement (S.Perform name) = do 
  mpara <- uses envParas (Map.lookup name)
  case mpara of 
    Nothing -> error $ "PERFORM - no paragraph with name: " ++ show name
    Just para -> runPara para
runStatement (S.PerformUntil cond sts) = performUntil cond sts

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

runCloseFd :: T.Text -> CI ()
runCloseFd fd_name = do 
  fd <- getFd fd_name
  case fd of 
    (Fd _ Nothing) -> error $ "CLOSE: FD is not open: " ++ show fd_name
    (Fd fname (Just handle)) -> do 
      lift $ hClose handle
      envFds %= Map.insert fd_name (Fd fname Nothing)

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
        then maybe (pure True) runSentence msts
        else pure True

readData :: Handle -> T.Text -> Data -> CI ()
readData h vname (StrData cs n _)   = do 
  txt <- lift $ E.readText h n
  envVars %= Map.insert vname (StrData cs n txt)
readData h vname (DecData cs d w _) = do
  dec <- lift $ E.readComp3 h d w
  envVars %= Map.insert vname (DecData cs d w dec)
readData h _ (GroupData child_names)  = do
  mapM_ byName child_names
  where 
    byName :: T.Text -> CI ()
    byName vname = do 
      vdata <- getVarData vname
      readData h vname vdata

runWrite :: T.Text -> Maybe T.Text -> Maybe Int -> CI ()
runWrite var_name mfrom_var madv = do 
  (Fd _ (Just handle)) <- getFdByVarName var_name
  sequence_ $ advanceLines handle <$> madv  -- handle AFTER ADVANCE LINES clause
  txt <- getVarData (fromMaybe var_name mfrom_var) >>= dataText
  lift $ TIO.hPutStrLn handle txt
  where 
    advanceLines :: Handle -> Int -> CI ()
    advanceLines h n = lift $ TIO.hPutStr h (T.replicate n "\n")

performUntil :: S.Cond -> [S.Statement] -> CI Bool
performUntil cond sts = do 
  cond_res <- runCond cond
  if cond_res
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
evalValue (S.VarVal var) = Right <$> getVarData var
evalValue (S.FunCall fname) = 
  case fname of 
    "CURRENT-DATE" -> Left . StrVal <$> getCurrentDate
    _ -> error $ "Unknown function: " ++ show fname

-- Return the current date formatted to the COBOL standard format.
-- see: https://www.ibm.com/docs/en/cobol-zos/6.4?topic=functions-current-date
-- COBOL FMT: %Y%m%d%H%M%S00%z (the zeros should be hundredths of a second, but not implemented)
getCurrentDate :: CI T.Text
getCurrentDate = do 
  now <- lift getZonedTime
  pure . T.pack $ formatTime defaultTimeLocale "%Y%m%d%H%M%S00%z" now

dataText :: Data -> CI T.Text
dataText (StrData _ l v)     = pure $ rightPadT l v
dataText (DecData fmt _ _ v) = pure $ decText fmt v
dataText (GroupData cnames) = 
  T.concat <$> traverse (getVarData >=> dataText) cnames

rightPadT :: Int -> T.Text -> T.Text
rightPadT n txt 
  | n > len   = txt <> T.replicate (n - len) " " 
  | otherwise = txt
  where 
    len = T.length txt

rightPadWith :: a -> Int -> [a] -> [a]
rightPadWith x n xs 
  | n > len = xs ++ replicate (n - len) x
  | otherwise = xs
  where 
    len = length xs

decText :: [S.RFmtChar] -> Double -> T.Text
decText chars val = 
  let (whole_cs, part_cs') = break isDecPt chars 
      part_cs = drop 1 part_cs'

      (whole_ds, part_ds) = toDigits val

      sign_txt  = if val < 0 then "-" else ""
      whole_txt = T.reverse $ renderDigits (reverse whole_cs) (reverse whole_ds)
      part_txt  = if null part_cs then "" else "." <> renderDigits part_cs (part_ds ++ repeat 0)
   in sign_txt <> whole_txt <> part_txt
  where 
    -- Positive double to whole and part digits
    toDigits :: Double -> ([Int], [Int])
    toDigits n = 
      let (digits, e) = floatToDigits 10 (abs n)
       in if e <= length digits 
            then let (w, p) = splitAt e digits in (w, drop 1 p)
            else (rightPadWith 0 e digits, [])

    renderDigits :: [S.RFmtChar] -> [Int] -> T.Text
    renderDigits [] _ = ""  -- TODO this will truncate numbers that are too big (maybe it's ok? call it 'undefined behavior')
    renderDigits (c:cs) []
      | isCurrencySym c = "$" <> T.replicate (length cs) " " 
      | S.RFNum == c    = T.replicate (length (c:cs)) " "
      | otherwise = error $ "Cannot render chars, no more digits: " ++ show (c:cs)
    renderDigits (c:cs) (d:ds) 
      | isNumericDigit c = showT d <> renderDigits cs ds
      | S.RFComma == c   = ','     `T.cons` renderDigits cs (d:ds)
      | otherwise        = error $ "Cannot render digit as format char: " ++ show c

    isDecPt :: S.RFmtChar -> Bool
    isDecPt S.RFDec    = True
    isDecPt S.RFDecPer = True
    isDecPt _ = False

    isCurrencySym :: S.RFmtChar -> Bool 
    isCurrencySym S.RFCurrency = True
    isCurrencySym S.RFComma = True
    isCurrencySym _ = False

    isNumericDigit :: S.RFmtChar -> Bool
    isNumericDigit S.RFNum      = True
    isNumericDigit S.RFCurrency = True
    isNumericDigit _ = False

valueText :: Value -> CI T.Text
valueText (StrVal v) = pure v
valueText (DecVal v) = pure $ showT v
valueText (GroupVal cnames) = 
  T.concat <$> traverse (getVarData >=> dataText) cnames

mapMWhile_ :: (a -> CI Bool) -> [a] -> CI ()
mapMWhile_ f xs = () <$ mapMWhile f xs

mapMWhile :: (a -> CI Bool) -> [a] -> CI Bool
mapMWhile _ [] = pure True
mapMWhile f (x:xs) = do 
  res <- f x
  if res then mapMWhile f xs else pure False

_test_report :: IO ()
_test_report = 
  interpretFile 
    [ ( "PRTLINE", "examples/report/prtline" )
    , ("ACCTREC", "examples/report/data")
    ] 
    "examples/report/REPORT.CBL"