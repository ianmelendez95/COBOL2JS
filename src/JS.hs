{-# LANGUAGE OverloadedStrings #-}

module JS where 

import qualified Data.Text as T

newtype Script = Script [Statement]

data Statement = Log T.Text

scriptToText :: Script -> T.Text
scriptToText (Script statements) = T.unlines . map statementToText $ statements

statementToText :: Statement -> T.Text
statementToText (Log msg) = "console.log(\"" <> msg <> "\");" 
