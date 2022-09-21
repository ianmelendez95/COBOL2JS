module COBOL.EBCDIC 
 ( readText
 , readComp3
 ) where 

import System.IO

import qualified Data.ByteString as BS
import qualified Data.Text as T

readText :: Handle -> Int -> IO T.Text
readText = undefined

readComp3 :: Handle -> Int -> Int -> IO Double
readComp3 = undefined
