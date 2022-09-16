{-# LANGUAGE OverloadedStrings #-}


module COBOL.Interpreter where 

import Control.Monad.State.Lazy
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import COBOL

type CI = StateT () IO

interpretFile :: FilePath -> IO ()
interpretFile file = COBOL.readFile file >>= interpret 

interpret :: Prog -> IO ()
interpret prog = evalStateT (runProg prog) ()

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
runStatement (Display values) = do
  txts <- traverse valueText values
  lift $ mapM_ TIO.putStr txts >> putChar '\n'

valueText :: Value -> CI T.Text
valueText (StrVal txt) = pure txt
