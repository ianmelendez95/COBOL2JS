{-# LANGUAGE OverloadedStrings #-}

module COBOL2JS where 

import qualified COBOL as COBOL
import qualified JS as JS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

c2jFile :: FilePath -> FilePath -> IO ()
c2jFile cobolSrc jsDest = do
  jsScript <- c2j <$> COBOL.readFile cobolSrc
  TIO.writeFile jsDest (JS.scriptToText jsScript)

c2j :: COBOL.Prog -> JS.Script
c2j (COBOL.Prog _ _ sts) = JS.Script (map statement2js sts)

statement2js :: COBOL.Statement -> JS.Statement
statement2js (COBOL.Display vs) = JS.Log (map c2jVal vs)
statement2js (COBOL.Move val var) = JS.Set (c2jVar var) (JS.AVal (c2jVal val))
statement2js (COBOL.Compute var val) = JS.Set (c2jVar var) (c2jArith val)

c2jVal :: COBOL.Value -> JS.Value
c2jVal (COBOL.VarVal v) = JS.VarVal (c2jVar v)
c2jVal (COBOL.NumVal v) = JS.NumVal v
c2jVal (COBOL.StrVal v) = JS.StrVal v

c2jVar :: T.Text -> T.Text
c2jVar = T.replace "-" "_" . T.toLower

c2jArith :: COBOL.Arith -> JS.Arith
c2jArith (COBOL.AVal v) = JS.AVal (c2jVal v)
c2jArith (COBOL.Mult v1 v2) = JS.Mult (c2jArith v1) (c2jArith v2)