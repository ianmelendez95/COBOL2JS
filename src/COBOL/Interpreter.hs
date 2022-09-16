{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module COBOL.Interpreter where 

import Control.Lens

import Control.Monad.State.Lazy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text.Util
import COBOL

type CI = StateT CIEnv IO

data CIEnv = CIEnv 
  { _envVars :: Map T.Text Value
  }

makeLenses ''CIEnv

instance Semigroup CIEnv where 
  (CIEnv vars1) <> (CIEnv vars2) = CIEnv (vars1 <> vars2)

instance Monoid CIEnv where 
  mempty = CIEnv mempty


interpretFile :: FilePath -> IO ()
interpretFile file = COBOL.readFile file >>= interpret 

interpret :: Prog -> IO ()
interpret prog = evalStateT (runProg prog) mempty

runProg :: Prog -> CI ()
runProg (Prog _ _ _ proc_div) = runProcDiv proc_div

runProcDiv :: ProcDiv -> CI ()
runProcDiv = mapM_ runPara

runPara :: Para -> CI ()
runPara (Para _ sents) = mapM_ runSentence sents

runSentence :: Sentence -> CI ()
runSentence = mapM_ runStatement

runStatement :: Statement -> CI ()
runStatement GoBack = pure ()
runStatement (Move val var) = envVars %= Map.insert var val
runStatement (Compute var arith) = do
  arith_val <- runArith arith
  envVars %= Map.insert var arith_val
runStatement (Display values) = do
  txts <- traverse valueText values
  lift $ mapM_ TIO.putStr txts >> putChar '\n'

runArith :: Arith -> CI Value
runArith (AVal val) = pure val
runArith (ABin a1 aop a2) = 
  arithOpFunc aop <$> (runArith a1 >>= evalValue) 
                  <*> (runArith a2 >>= evalValue)
  where
    arithOpFunc :: IOp -> (Value -> Value -> Value)
    arithOpFunc Mult = numValueFunc (*)  
    arithOpFunc Add  = numValueFunc (+)  
    arithOpFunc aop'  = error $ "Arithmetic does not support '" ++ show aop' ++ "' operator"

    numValueFunc :: (Int -> Int -> Int) -> Value -> Value -> Value
    numValueFunc f (NumVal x) (NumVal y) = NumVal $ f x y
    numValueFunc _ (NumVal _) v = error $ "Second argument is not a number: " ++ show v
    numValueFunc _ v _          = error $ "First argument is not a number: " ++ show v

evalValue :: Value -> CI Value
evalValue str@(StrVal _) = pure str
evalValue num@(NumVal _) = pure num
evalValue (VarVal var) = do 
  mval <- uses envVars (Map.lookup var)
  case mval of 
    Nothing -> error $ "Var has no value: " ++ show var
    Just (VarVal val) -> error $ "Var has var value: " ++ show var ++ " => " ++ show val
    Just val -> pure val

valueText :: Value -> CI T.Text
valueText (StrVal txt) = pure txt
valueText (NumVal n) = pure $ showT n
valueText var@(VarVal _) = evalValue var >>= valueText

