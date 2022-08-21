module COBOL2JS where 

import qualified COBOL as COBOL
import qualified JS as JS
import qualified Data.Text.IO as T

cobol2jsFile :: FilePath -> FilePath -> IO ()
cobol2jsFile cobolSrc jsDest = do
  jsScript <- cobol2js <$> COBOL.readFile cobolSrc
  T.writeFile jsDest (JS.scriptToText jsScript)

cobol2js :: COBOL.Prog -> JS.Script
cobol2js (COBOL.Prog _ _ sts) = JS.Script (map statement2js sts)

statement2js :: COBOL.Statement -> JS.Statement
statement2js (COBOL.Display txt) = JS.Log txt